use std::collections::BTreeSet;

use chumsky::prelude::*;

use crate::ast::{Literal, TypeName, Node, Value, Integer, Decimal, Radix};
use crate::ast::{SpannedName, SpannedChildren, Document};
use crate::span::Span;
use crate::errors::{ParseErrorEnum as Error, TokenFormat};


fn newline() -> impl Parser<char, (), Error=Error> {
    just('\r')
        .or_not()
        .ignore_then(just('\n'))
        .or(just('\r')) // Carriage return
        .or(just('\x0C')) // Form feed
        .or(just('\u{0085}')) // Next line
        .or(just('\u{2028}')) // Line separator
        .or(just('\u{2029}')) // Paragraph separator
        .ignored()
}

fn ws_char() -> impl Parser<char, (), Error=Error> {
    filter(|c| matches!(c,
        '\t' | ' ' | '\u{00a0}' | '\u{1680}' |
        '\u{2000}'..='\u{200A}' |
        '\u{202F}' | '\u{205F}' | '\u{3000}' |
        '\u{FEFF}'
    ))
    .ignored()
}

fn ws() -> impl Parser<char, (), Error=Error> {
    ws_char().repeated().at_least(1).ignored().or(ml_comment())
}

fn comment() -> impl Parser<char, (), Error=Error> {
    just("//").then(take_until(newline().or(end()))).ignored()
}

fn ml_comment() -> impl Parser<char, (), Error=Error> {
    recursive::<_, _, _, _, Error>(|comment| {
        choice((
            comment,
            none_of('*').ignored(),
            just('*').then_ignore(none_of('/').rewind()).ignored(),
        )).repeated().ignored()
        .delimited_by(just("/*"), just("*/")).ignored()
    })
    .map_err_with_span(|e, span| {
        if matches!(&e, Error::Unexpected { found: TokenFormat::Eoi, .. }) &&
           span.length() > 2
        {
            assert!(span.1 - span.0 >= 2);
            e.merge(Error::Unclosed {
                label: "comment",
                opened_at: Span(span.0, span.0+2), // we know it's `/ *`
                opened: "/*".into(),
                expected_at: Span(span.1, span.1),
                expected: "*/".into(),
                found: None.into(),
            })
        } else {
            // otherwise opening /* is not matched
            e
        }
    })
}

fn raw_string() -> impl Parser<char, Box<str>, Error=Error> {
    just('r')
        .ignore_then(just('#').repeated().map(|v| v.len()))
        .then_ignore(just('"'))
        .then_with(|sharp_num|
            take_until(
                just('"')
                .ignore_then(just('#').repeated().exactly(sharp_num)
                             .ignored()))
            .map_err_with_span(move |e, span| {
                if matches!(&e, Error::Unexpected {
                    found: TokenFormat::Eoi, .. })
                {
                    e.merge(Error::Unclosed {
                        label: "raw string",
                        opened_at: Span(span.0 - sharp_num - 2, span.0),
                        opened: TokenFormat::OpenRaw(sharp_num),
                        expected_at: Span(span.1, span.1),
                        expected: TokenFormat::CloseRaw(sharp_num),
                        found: None.into(),
                    })
                } else {
                    e
                }
            })
        )
    .map(|(text, ())| {
        text.into_iter().collect::<String>().into()
    })
}

fn string() -> impl Parser<char, Box<str>, Error=Error> {
    raw_string().or(escaped_string())
}

fn expected(s: &'static str) -> BTreeSet<TokenFormat> {
    [TokenFormat::Kind(s)].into_iter().collect()
}

fn esc_char() -> impl Parser<char, char, Error=Error> {
    filter_map(|position, c| match c {
        '"'|'\\'|'/' => Ok(c),
        'b' => Ok('\u{0008}'),
        'f' => Ok('\u{000C}'),
        'n' => Ok('\n'),
        'r' => Ok('\r'),
        't' => Ok('\t'),
        c => Err(Error::Unexpected {
            label: Some("invalid escape char"),
            position,
            found: c.into(),
            expected: "\"\\/bfnrt".chars().map(|c| c.into()).collect(),
        })
    })
    .or(just('u').ignore_then(
            filter_map(|position, c: char| c.is_digit(16).then(|| c)
                .ok_or_else(|| Error::Unexpected {
                    label: Some("unexpected character"),
                    position,
                    found: c.into(),
                    expected: expected("hexadecimal digit"),
                }))
            .repeated()
            .at_least(1)
            .at_most(6)
            .delimited_by(just('{'), just('}'))
            .try_map(|hex_chars, position| {
                let s = hex_chars.into_iter().collect::<String>();
                let c =
                    u32::from_str_radix(&s, 16).map_err(|e| e.to_string())
                    .and_then(|n| char::try_from(n).map_err(|e| e.to_string()))
                    .map_err(|e| Error::ParseError {
                        label: Some("invalid character code"),
                        position,
                        message: e.to_string(),
                    })?;
                Ok(c)
            })
            .recover_with(skip_until(['}', '"', '\\'], |_| '\0'))))
}

fn escaped_string() -> impl Parser<char, Box<str>, Error=Error> {
    filter(|&c| c != '"' && c != '\\')
    .or(just('\\').ignore_then(esc_char()))
    .repeated()
    .delimited_by(just('"'), just('"'))
    .map(|val| val.into_iter().collect::<String>().into())
    .map_err_with_span(|e, span| {
        if matches!(&e, Error::Unexpected { found: TokenFormat::Eoi, .. }) {
            e.merge(Error::Unclosed {
                label: "string",
                opened_at: Span(span.0, span.0+1), // we know it's `"`
                opened: '"'.into(),
                expected_at: Span(span.1, span.1),
                expected: '"'.into(),
                found: None.into(),
            })
        } else {
            e
        }
    })
}

/*
fn parser<S: Span>() -> impl Parser<char, Document<S>, Error=Simple<char>> {
    todo!()
}
*/

#[cfg(test)]
mod test {
    use chumsky::prelude::*;
    use chumsky::error::SimpleReason;
    use chumsky::Stream;
    use crate::errors::{ParseError, ParseErrorEnum, AddSource};
    use crate::span::Span;
    use super::{ws, comment, ml_comment, string};

    macro_rules! err_eq {
        ($left: expr, $right: expr) => {
            let left = $left.unwrap_err();
            let left = left.replace("}{", "},{"); // bug in miette master
            let left = left.replace(r#"\\""#, r#"\""#); // bug in miette
            let left: serde_json::Value = serde_json::from_str(&left).unwrap();
            let right: serde_json::Value =
                serde_json::from_str($right).unwrap();
            //assert_json_diff::assert_json_includes!(
            //    actual: left, expected: right);
            assert_json_diff::assert_json_eq!(left, right);
        }
    }

    fn parse<'x, P, T>(p: P, text: &'x str) -> Result<T, String>
        where P: Parser<char, T, Error=ParseErrorEnum>
    {
        p.then_ignore(end())
        .parse(Stream::from_iter(
                Span(text.len(), text.len()),
                text.char_indices()
                    .map(|(i, c)| (c, Span(i, i + c.len_utf8()))),
        )).map_err(|errors| {
            let source: std::sync::Arc<String> = (text.to_string() + " ").into();
            let e = ParseError {
                errors: errors.into_iter().map(|error| {
                    AddSource {
                        source: source.clone(),
                        error,
                    }
                }).collect(),
            };
            let mut buf = String::with_capacity(512);
            miette::GraphicalReportHandler::new()
                .render_report(&mut buf, &e).unwrap();
            println!("{}", buf);
            buf.truncate(0);
            miette::JSONReportHandler::new()
                .render_report(&mut buf, &e).unwrap();
            return buf;
        })
    }

    #[test]
    fn parse_ws() {
        parse(ws(), "   ").unwrap();
        parse(ws(), "text").unwrap_err();
    }

    #[test]
    fn parse_comments() {
        parse(comment(), "//hello").unwrap();
        parse(comment(), "//hello\n").unwrap();
        parse(ml_comment(), "/*nothing*/").unwrap();
        parse(ml_comment(), "/*nothing**/").unwrap();
        parse(ml_comment(), "/*no*thing*/").unwrap();
        parse(ml_comment(), "/*no/**/thing*/").unwrap();
        parse(ml_comment(), "/*no/*/**/*/thing*/").unwrap();
        parse(ws().then(comment()), "   // hello").unwrap();
        parse(ws().then(comment()).then(ws()).then(comment()),
              "   // hello\n   //world").unwrap();
    }

    #[test]
    fn parse_comment_err() {
        err_eq!(parse(ws(), r#"/* comment"#), r#"{
            "message": "error parsing KDL text",
            "severity": "error",
            "labels": [],
            "related": [{
                "message": "unclosed comment `/*`",
                "severity": "error",
                "filename": "",
                "labels": [
                    {"label": "opened here",
                    "span": {"offset": 0, "length": 2}},
                    {"label": "expected `*/`",
                    "span": {"offset": 10, "length": 0}}
                ],
                "related": []
            }]
        }"#);
        err_eq!(parse(ws(), r#"/* com/*ment *"#), r#"{
            "message": "error parsing KDL text",
            "severity": "error",
            "labels": [],
            "related": [{
                "message": "unclosed comment `/*`",
                "severity": "error",
                "filename": "",
                "labels": [
                    {"label": "opened here",
                    "span": {"offset": 0, "length": 2}},
                    {"label": "expected `*/`",
                    "span": {"offset": 14, "length": 0}}
                ],
                "related": []
            }]
        }"#);
        err_eq!(parse(ws(), r#"/* com/*me*/nt *"#), r#"{
            "message": "error parsing KDL text",
            "severity": "error",
            "labels": [],
            "related": [{
                "message": "unclosed comment `/*`",
                "severity": "error",
                "filename": "",
                "labels": [
                    {"label": "opened here",
                    "span": {"offset": 0, "length": 2}},
                    {"label": "expected `*/`",
                    "span": {"offset": 16, "length": 0}}
                ],
                "related": []
            }]
        }"#);
        err_eq!(parse(ws(), r#"/* comment *"#), r#"{
            "message": "error parsing KDL text",
            "severity": "error",
            "labels": [],
            "related": [{
                "message": "unclosed comment `/*`",
                "severity": "error",
                "filename": "",
                "labels": [
                    {"label": "opened here",
                    "span": {"offset": 0, "length": 2}},
                    {"label": "expected `*/`",
                    "span": {"offset": 12, "length": 0}}
                ],
                "related": []
            }]
        }"#);
        err_eq!(parse(ws(), r#"/*/"#), r#"{
            "message": "error parsing KDL text",
            "severity": "error",
            "labels": [],
            "related": [{
                "message": "unclosed comment `/*`",
                "severity": "error",
                "filename": "",
                "labels": [
                    {"label": "opened here",
                    "span": {"offset": 0, "length": 2}},
                    {"label": "expected `*/`",
                    "span": {"offset": 3, "length": 0}}
                ],
                "related": []
            }]
        }"#);
        // nothing is expected for comment or whitespace
        err_eq!(parse(ws(), r#"xxx"#), r#"{
            "message": "error parsing KDL text",
            "severity": "error",
            "labels": [],
            "related": [{
                "message": "found `x`, expected `/`",
                "severity": "error",
                "filename": "",
                "labels": [
                    {"label": "unexpected token",
                    "span": {"offset": 0, "length": 1}}
                ],
                "related": []
            }]
        }"#);
    }

    #[test]
    fn parse_str() {
        assert_eq!(&*parse(string(), r#""hello""#).unwrap(), "hello");
        assert_eq!(&*parse(string(), r#""""#).unwrap(), "");
        assert_eq!(&*parse(string(), r#""hel\"lo""#).unwrap(),"hel\"lo");
        assert_eq!(&*parse(string(), r#""hello\nworld!""#).unwrap(),
                   "hello\nworld!");
        assert_eq!(&*parse(string(), r#""\u{1F680}""#).unwrap(), "🚀");
    }

    #[test]
    fn parse_raw_str() {
        assert_eq!(&*parse(string(), r#"r"hello""#).unwrap(), "hello");
        assert_eq!(&*parse(string(), r##"r#"world"#"##).unwrap(), "world");
        assert_eq!(&*parse(string(), r##"r#"world"#"##).unwrap(), "world");
        assert_eq!(&*parse(string(), r####"r###"a\n"##b"###"####).unwrap(),
                   "a\\n\"##b");
    }

    #[test]
    fn parse_str_err() {
        err_eq!(parse(string(), r#""hello"#), r#"{
            "message": "error parsing KDL text",
            "severity": "error",
            "labels": [],
            "related": [{
                "message": "unclosed string `\"`",
                "severity": "error",
                "filename": "",
                "labels": [
                    {"label": "opened here",
                    "span": {"offset": 0, "length": 1}},
                    {"label": "expected `\"`",
                    "span": {"offset": 6, "length": 0}}
                ],
                "related": []
            }]
        }"#);
        err_eq!(parse(string(), r#""he\u{FFFFFF}llo""#), r#"{
            "message": "error parsing KDL text",
            "severity": "error",
            "labels": [],
            "related": [{
                "message": "converted integer out of range for `char`",
                "severity": "error",
                "filename": "",
                "labels": [
                    {"label": "unexpected token",
                    "span": {"offset": 5, "length": 8}}
                ],
                "related": []
            }]
        }"#);
        err_eq!(parse(string(), r#""he\u{1234567}llo""#), r#"{
            "message": "error parsing KDL text",
            "severity": "error",
            "labels": [],
            "related": [{
                "message": "found `7`, expected `}`",
                "severity": "error",
                "filename": "",
                "labels": [
                    {"label": "unexpected token",
                    "span": {"offset": 12, "length": 1}}
                ],
                "related": []
            }]
        }"#);
        err_eq!(parse(string(), r#""he\u{1gh}llo""#), r#"{
            "message": "error parsing KDL text",
            "severity": "error",
            "labels": [],
            "related": [{
                "message": "found `g`, expected `}` or hexadecimal digit",
                "severity": "error",
                "filename": "",
                "labels": [
                    {"label": "unexpected token",
                    "span": {"offset": 7, "length": 1}}
                ],
                "related": []
            }]
        }"#);
        err_eq!(parse(string(), r#""he\x01llo""#), r#"{
            "message": "error parsing KDL text",
            "severity": "error",
            "labels": [],
            "related": [{
                "message":
                    "found `x`, expected `\"`, `/`, `\\`, `b`, `f`, `n`, `r`, `t` or `u`",
                "severity": "error",
                "filename": "",
                "labels": [
                    {"label": "invalid escape char",
                    "span": {"offset": 4, "length": 1}}
                ],
                "related": []
            }]
        }"#);
        // Tests error recovery
        err_eq!(parse(string(), r#""he\u{FFFFFF}l\!lo""#), r#"{
            "message": "error parsing KDL text",
            "severity": "error",
            "labels": [],
            "related": [{
                "message": "converted integer out of range for `char`",
                "severity": "error",
                "filename": "",
                "labels": [
                    {"label": "unexpected token",
                    "span": {"offset": 5, "length": 8}}
                ],
                "related": []
            }, {
                "message":
                    "found `!`, expected `\"`, `/`, `\\`, `b`, `f`, `n`, `r`, `t` or `u`",
                "severity": "error",
                "filename": "",
                "labels": [
                    {"label": "invalid escape char",
                    "span": {"offset": 15, "length": 1}}
                ],
                "related": []
            }]
        }"#);
    }
    #[test]
    fn parse_raw_str_err() {
        err_eq!(parse(string(), r#"r"hello"#),  r#"{
            "message": "error parsing KDL text",
            "severity": "error",
            "labels": [],
            "related": [{
                "message": "unclosed raw string `r\"`",
                "severity": "error",
                "filename": "",
                "labels": [
                    {"label": "opened here",
                    "span": {"offset": 0, "length": 2}},
                    {"label": "expected `\"`",
                    "span": {"offset": 7, "length": 0}}
                ],
                "related": []
            }]
        }"#);
        err_eq!(parse(string(), r###"r#"hello""###), r###"{
            "message": "error parsing KDL text",
            "severity": "error",
            "labels": [],
            "related": [{
                "message": "unclosed raw string `r#\"`",
                "severity": "error",
                "filename": "",
                "labels": [
                    {"label": "opened here",
                    "span": {"offset": 0, "length": 3}},
                    {"label": "expected `\"#`",
                    "span": {"offset": 9, "length": 0}}
                ],
                "related": []
            }]
        }"###);
        err_eq!(parse(string(), r####"r###"hello"####), r####"{
            "message": "error parsing KDL text",
            "severity": "error",
            "labels": [],
            "related": [{
                "message": "unclosed raw string `r###\"`",
                "severity": "error",
                "filename": "",
                "labels": [
                    {"label": "opened here",
                    "span": {"offset": 0, "length": 5}},
                    {"label": "expected `\"###`",
                    "span": {"offset": 10, "length": 0}}
                ],
                "related": []
            }]
        }"####);
        err_eq!(parse(string(), r####"r###"hello"#world"####), r####"{
            "message": "error parsing KDL text",
            "severity": "error",
            "labels": [],
            "related": [{
                "message": "unclosed raw string `r###\"`",
                "severity": "error",
                "filename": "",
                "labels": [
                    {"label": "opened here",
                    "span": {"offset": 0, "length": 5}},
                    {"label": "expected `\"###`",
                    "span": {"offset": 17, "length": 0}}
                ],
                "related": []
            }]
        }"####);
    }
}

