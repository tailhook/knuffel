use chumsky::prelude::*;

use crate::ast::{Literal, TypeName, Node, Value, Integer, Decimal, Radix};
use crate::ast::{SpannedName, SpannedChildren, Document};
use crate::traits::Span;
use crate::errors::ParseErrorEnum as Error;


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
    recursive(|comment| {
        choice((
            comment,
            none_of('*').ignored(),
            just('*').then_ignore(none_of('/').rewind()).ignored(),
        )).repeated().ignored()
        .delimited_by(just("/*"), just("*/")).ignored()
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
    use super::{ws, comment, ml_comment};
    use miette::IntoDiagnostic;

    macro_rules! err_eq {
        ($left: expr, $right: expr) => {
            let left: serde_json::Value =
                serde_json::from_str(&$left.unwrap_err()).unwrap();
            let right: serde_json::Value =
                serde_json::from_str($right).unwrap();
            pretty_assertions::assert_eq!(left, right);
        }
    }

    fn parse<'x, P, T>(p: P, text: &'x str) -> Result<T, String>
        where P: Parser<char, T, Error=ParseErrorEnum>
    {
        let (data, errors) = p.then_ignore(end())
            .parse_recovery(Stream::from_iter(
                Span(text.len(), text.len()),
                text.char_indices()
                    .map(|(i, c)| (c, Span(i, i + c.len_utf8()))),
            ));
        if !errors.is_empty() {
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
            return Err(buf);
        }
        if let Some(data) = data {
            return Ok(data);
        }
        unreachable!();
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
                "message": "found end of input, expected `*` or `/`",
                "severity": "error",
                "filename": "",
                "labels": [
                    {"label": "unexpected token",
                    "span": {"offset": 10,"length": 0}}
                ],
                "related": []
            }]
        }"#);
        err_eq!(parse(ws(), r#"/* comment *"#),
            "{}");
        err_eq!(parse(ws(), r#"/*/"#),
                "{}");
        // nothing is expected for comment or whitespace
        err_eq!(parse(ws(), r#"xxx"#),
                "{}");
    }
}
