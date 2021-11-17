use std::collections::BTreeMap;

use combine::error::StreamError;
use combine::parser::combinator::{recognize, no_partial};
use combine::parser::char::{digit, oct_digit, hex_digit};
use combine::parser::repeat::{repeat_skip_until, repeat_until, skip_until};
use combine::stream::state;
use combine::stream::{PointerOffset, StreamErrorFor};
use combine::{Stream, Parser};
use combine::{eof, optional, between, position, any, choice};
use combine::{opaque, attempt, count_min_max, count};
use combine::{many, skip_many1, skip_many, satisfy, satisfy_map, parser};

use crate::ast::{Literal, TypeName, Node, Value, Integer, Radix};
use crate::ast::{SpannedName, SpannedChildren};
use crate::span::{Spanned, SpanContext};

struct SpanParser<P>(P);

pub struct SpanState<'a, S: SpanContext<usize>> {
    span_context: S,
    data: &'a str,
}

trait SpanStream: Stream {
    type Span;
    fn span_from(&self, start: Self::Position) -> Self::Span;
}

enum PropOrArg<S> {
    Prop(SpannedName<S>, Value<S>),
    Arg(Value<S>),
}

fn token<I: Stream<Token=char>>(val: char) -> impl Parser<I, Output=()> {
    combine::token(val).map(|_| ())
}

fn ws_char<I: Stream<Token=char>>() -> impl Parser<I, Output=()> {
    satisfy(|c| matches!(c,
        '\t' | ' ' | '\u{00a0}' | '\u{1680}' |
        '\u{2000}'..='\u{200A}' |
        '\u{202F}' | '\u{205F}' | '\u{3000}' |
        '\u{FEFF}'
    ))
    .map(|_| ())
}

fn id_sans_sign_dig<I: Stream<Token=char>>() -> impl Parser<I, Output=char> {
    satisfy(|c| !matches!(c,
        '-'| '+' | '0'..='9' |
        '\u{0000}'..='\u{0020}' |
        '\\'|'/'|'('|')'|'{'|'}'|'<'|'>'|';'|'['|']'|'='|','|'"' |
        // whitespace, excluding 0x20
        '\u{00a0}' | '\u{1680}' |
        '\u{2000}'..='\u{200A}' |
        '\u{202F}' | '\u{205F}' | '\u{3000}' |
        // newline (excluding <= 0x20)
        '\u{0085}' | '\u{2028}' | '\u{2029}'
    ))
}

fn id_sans_dig<I: Stream<Token=char>>() -> impl Parser<I, Output=char> {
    satisfy(|c| !matches!(c,
        '0'..='9' |
        '\u{0000}'..='\u{0020}' |
        '\\'|'/'|'('|')'|'{'|'}'|'<'|'>'|';'|'['|']'|'='|','|'"' |
        // whitespace, excluding 0x20
        '\u{00a0}' | '\u{1680}' |
        '\u{2000}'..='\u{200A}' |
        '\u{202F}' | '\u{205F}' | '\u{3000}' |
        // newline (excluding <= 0x20)
        '\u{0085}' | '\u{2028}' | '\u{2029}'
    ))
}

fn id_char<I: Stream<Token=char>>() -> impl Parser<I, Output=char> {
    satisfy(|c| !matches!(c,
        '\u{0000}'..='\u{0021}' |
        '\\'|'/'|'('|')'|'{'|'}'|'<'|'>'|';'|'['|']'|'='|','|'"' |
        // whitespace, excluding 0x20
        '\u{00a0}' | '\u{1680}' |
        '\u{2000}'..='\u{200A}' |
        '\u{202F}' | '\u{205F}' | '\u{3000}' |
        // newline (excluding <= 0x20)
        '\u{0085}' | '\u{2028}' | '\u{2029}'
    ))
}

fn newline<I: Stream<Token=char>>() -> impl Parser<I, Output=()> {
    token('\r').skip(optional(token('\n')))
    .or(satisfy(|c| matches!(c,
        '\n' | '\u{0085}' | '\u{000C}' | '\u{2028}' | '\u{2029}'))
        .map(|_| ()))
    .silent().expected("newline")
}

fn not_nl_char<I: Stream<Token=char>>() -> impl Parser<I, Output=()> {
    satisfy(|c| !matches!(c,
        '\r' | '\n' | '\u{0085}' | '\u{000C}' | '\u{2028}' | '\u{2029}'))
    .map(|_| ())
}

fn comment<I: Stream<Token=char>>() -> impl Parser<I, Output=()> {
    (attempt((token('/'), token('/'))),
        skip_many(not_nl_char()), newline().or(eof()))
    .silent()
    .map(|_| ())
}

fn ml_comment<I: Stream<Token=char>>() -> impl Parser<I, Output=()> {
    (
        attempt((token('/'), token('*')).silent()),
        opaque!(no_partial(
            repeat_skip_until(
                ml_comment().or(any().map(|_| ())),
                attempt((token('*'), token('/'))).map(|_| ()).or(eof()),
            )
        )),
        (token('*'), token('/')).expected("*/").message("unclosed comment"),
    ).map(|_| ())
}

fn ws<I: Stream<Token=char>>() -> impl Parser<I, Output=()> {
    skip_many1(ws_char().silent()).or(ml_comment())
}

fn esc_value<I: Stream<Token=char>>() -> impl Parser<I, Output=char> {
    struct UniEscape(u32);
    impl std::default::Default for UniEscape {
        fn default() -> UniEscape { UniEscape(0) }
    }
    impl Extend<u32> for UniEscape {
        fn extend<T: IntoIterator<Item=u32>>(&mut self, iter: T) {
            for item in iter {
                self.0 = (self.0 << 4) | item;
            }
        }
    }

    satisfy_map(|c| match c {
        '"'|'\\'|'/' => Some(c),
        'b' => Some('\u{0008}'),
        'f' => Some('\u{000C}'),
        'n' => Some('\n'),
        'r' => Some('\r'),
        't' => Some('\t'),
        _ => None,
    }).message("invalid escape char")
    .or(token('u').and(token('{')).with(
        count_min_max(1, 6, satisfy_map(|x: char| x.to_digit(16))))
        .skip(
            token('}').message("unicode escape should contain \
                               1 to 6 hexadecimal chars"))
        .and_then(|code: UniEscape| {
            code.0.try_into().map_err(|_| {
                StreamErrorFor::<I>::message_static_message(
                    "unicode escape out of range")
            })
        }))
}

fn escaped_string<I: Stream<Token=char>>() -> impl Parser<I, Output=Box<str>> {
    between(token('"'), token('"').message("unclosed quoted string"), many(
        satisfy(|c| c != '"' && c != '\\')
        .or(token('\\').with(esc_value()))
        .silent()  // expose only unexpected '"'
    ))
    .map(|val: String| val.into())
}

fn raw_string<I: Stream<Token=char>>() -> impl Parser<I, Output=Box<str>> {
    choice((
        between(attempt((token('r'), token('"'))).silent(),
                token('"').message("unclosed raw string"),
                many(satisfy(|c| c != '"'))),
        attempt((token('r'), token('#'))).silent()
            .with(parser(|input| {
                let mut iter = token('#').iter(input);
                let cnt = (&mut iter).count();
                iter.into_result(cnt + 1)
            })).skip(token('"'))
            .then(|n| {
                let mut quote = String::with_capacity(n+1);
                quote.push('"');
                for _ in 0..n {
                    quote.push('#');
                }
                let until = token('"')
                    .with(count_min_max::<(), _, _>(n, n, token('#')))
                    .or(eof());
                let end = token('"')
                    .with(count_min_max::<(), _, _>(n, n, token('#')))
                    .silent()
                    .expected(combine::error::Info::Format(quote))
                    .message("unclosed raw string");
                recognize(skip_until(attempt(until))).skip(end)
            }),
    )).map(|val: String| val.into())
}

fn string<I: Stream<Token=char>>() -> impl Parser<I, Output=Box<str>> {
    raw_string().or(escaped_string())
}

fn num_seq<I: Stream<Token=char>, P>(sign: Option<char>, digit: fn() -> P)
    -> impl Parser<I, Output=String>
    where P: Parser<I, Output=char>
{
    digit().then(move |first| {
        parser(move |input| {
            let mut s = String::new();
            sign.map(|c| s.push(c));
            s.push(first);
            let parser = digit().map(Some) .or(token('_').map(|_| None));
            let mut iter = parser.iter(input);
            s.extend((&mut iter).flat_map(|x| x));
            iter.into_result(s)
        })
    })
}

fn opt_sign<I: Stream<Token=char>>() -> impl Parser<I, Output=Option<char>> {
    optional(choice((
        combine::token('+'),
        combine::token('-'),
    )))
}

fn bin_digit<I: Stream<Token=char>>() -> impl Parser<I, Output=char> {
    combine::token('0').or(combine::token('1'))
}


fn radix_number<I: Stream<Token=char>>() -> impl Parser<I, Output=Literal> {
    choice((
        attempt(opt_sign().skip(token('0')).skip(token('b')))
            .then(|sign| num_seq(sign, bin_digit))
            .map(|s| Integer(Radix::Bin, s.into())),
        attempt(opt_sign().skip(token('0')).skip(token('o')))
            .then(|sign| num_seq(sign, oct_digit))
            .map(|s| Integer(Radix::Oct, s.into())),
        attempt(opt_sign().skip(token('0')).skip(token('x')))
            .then(|sign| num_seq(sign, hex_digit))
            .map(|s| Integer(Radix::Hex, s.into())),
    )).map(|val| Literal::Int(val))
}

fn number<I: Stream<Token=char>>() -> impl Parser<I, Output=Literal> {
    choice((
        radix_number(),
    ))
}


fn bare_ident<I: Stream<Token=char>>() -> impl Parser<I, Output=Box<str>> {
    let sign = token('+').or(token('-'));
    attempt(recognize(choice((
        (sign, id_sans_dig(), skip_many(id_char())).map(|_| ()),
        (id_sans_sign_dig(), skip_many(id_char())).map(|_| ()),
    ))).and_then(|s: String| {
        match &s[..] {
            "true" => Err(StreamErrorFor::<I>::unexpected("true")),
            "false" => Err(StreamErrorFor::<I>::unexpected("false")),
            "null" => Err(StreamErrorFor::<I>::unexpected("null")),
            _ => Ok(s.into()),
        }
    }).silent().expected("identifier"))
}

fn ident<I: Stream<Token=char>>() -> impl Parser<I, Output=Box<str>> {
    choice((bare_ident(), string()))
}

fn type_name<I: Stream<Token=char>>() -> impl Parser<I, Output=TypeName> {
    token('(').with(ident()).skip(token(')')).map(TypeName::from_string)
}

fn esc_line<I: Stream<Token=char>>() -> impl Parser<I, Output=()> {
    (token('\\'), skip_many(ws()), comment().or(newline())).map(|_| ())
}

fn line_space<I: Stream<Token=char>>() -> impl Parser<I, Output=()> {
    newline().or(ws()).or(comment()).silent()
}

fn node_space<I: Stream<Token=char>>() -> impl Parser<I, Output=()> {
    ws().or(esc_line()).map(|_| ()).silent()
}

fn children<I>() -> impl Parser<I, Output=SpannedChildren<I::Span>>
    where I: SpanStream<Token=char>,
{
    SpanParser(between(
        token('{').and(skip_many(line_space())),
        token('}').message("unclosed block"),
        repeat_until(
            opaque!(no_partial(SpanParser(node())))
                .skip(skip_many(line_space())),
            token('}').or(eof())),
    ))
}

impl<S> Extend<PropOrArg<S>> for Node<S> {
    fn extend<I: IntoIterator<Item=PropOrArg<S>>>(&mut self, iter: I) {
        use PropOrArg::*;
        for item in iter {
            match item {
                Prop(key, val) => {
                    self.properties.insert(key, val);
                }
                Arg(val) => {
                    self.arguments.push(val);
                }
            }
        }
    }
}

fn keyword<I: Stream<Token=char>>() -> impl Parser<I, Output=Literal> {
    use combine::parser::char::string as keyword;
    choice((
        keyword("null").map(|_| Literal::Null),
        keyword("true").map(|_| Literal::Bool(true)),
        keyword("false").map(|_| Literal::Bool(false)),
    ))
}

fn literal<I: Stream<Token=char>>() -> impl Parser<I, Output=Literal> {
    choice((
        string().map(Literal::String),
        keyword(),
        number(),
    ))
}

/// Untyped value without a string option, that is used to provide better
/// errors when value=something is written
fn arg_value<I>() -> impl Parser<I, Output=Value<I::Span>>
    where I: SpanStream<Token=char>,
{
    SpanParser(choice((
        keyword(),
        number(),
    ))).map(|literal| Value { type_name: None, literal })
}

fn value<I>() -> impl Parser<I, Output=Value<I::Span>>
    where I: SpanStream<Token=char>,
{
    combine::struct_parser!(
        Value {
            type_name: optional(SpanParser(type_name())),
            literal: SpanParser(literal()),
        }
    )
}

fn prop_or_arg<I>() -> impl Parser<I, Output=PropOrArg<I::Span>>
    where I: SpanStream<Token=char>,
{
    use PropOrArg::*;

    choice((
        SpanParser(bare_ident())
            .skip(token('=').message("bare identifiers cannot be used \
                as arguments; use double-quotes, \
                one of `true`, `false`, `null` or ensure it's a property by \
                adding `=` and value."))
            .and(value())
            .map(|(name, value)| Prop(name, value)),
        SpanParser(string()).and(optional(token('=').with(value())))
            .map(|(name, opt_value)| {
                if let Some(value) = opt_value {
                    Prop(name, value)
                } else {
                    Arg(Value {
                        type_name: None,
                        literal: name.map(Literal::String),
                    })
                }
            }),
        arg_value().map(Arg).skip(optional(
            token('=').and_then(|_| -> Result<(), StreamErrorFor<I>> {
                Err(StreamErrorFor::<I>::message_static_message(
                    "numbers and keywords cannot be used as property names, \
                    consider using double quotes"
                ))
            })
        )),
        value().map(Arg),
    ))
}

fn node_terminator<I: Stream<Token=char>>() -> impl Parser<I, Output=()> {
    choice((newline(), comment(), token(';'), eof()))
}

fn node<I>() -> impl Parser<I, Output=Node<I::Span>>
    where I: SpanStream<Token=char>,
{
    keyword().and_then(|_| Err(StreamErrorFor::<I>::message_static_message(
        "keyword cannot be used as a node name, \
         use double quotes or prepend the value with a node name"))
    ).or(
        combine::struct_parser!(
            Node {
                type_name: optional(SpanParser(type_name())),
                node_name: SpanParser(ident()),
                properties: combine::produce(BTreeMap::new),
                arguments: combine::produce(Vec::new),
                children: combine::produce(|| None),
            }
        )
    ).and(
        repeat_until(
            node_space().with(optional(prop_or_arg())),
            token('{').or(node_terminator()),
        )
    ).and(
        optional(children())
    ).skip(
        node_terminator()
    ).map(|((mut node, list), children): ((Node<_>, Vec<_>), _)| {
        node.extend(list.into_iter().flat_map(|opt| opt));
        node.children = children;
        node
    })
}

impl<'a, I, S> SpanStream for state::Stream<I, SpanState<'a, S>>
    where I: Stream<Position=PointerOffset<str>>,
          S: SpanContext<usize>,
{
    type Span = S::Span;
    fn span_from(&self, start: Self::Position) -> Self::Span {
        let start = start.translate_position(self.state.data);
        let end = self.stream.position().translate_position(self.state.data);
        return self.state.span_context.from_positions(start, end);
    }
}

impl<P: Parser<I>, I: SpanStream> Parser<I> for SpanParser<P> {
    type Output = Spanned<P::Output, I::Span>;
    type PartialState = ();

    #[inline]
    fn parse_lazy(&mut self, input: &mut I)
        -> combine::ParseResult<Self::Output, I::Error>
    {
        let start = input.position();
        self.0.parse_lazy(input)
            .map(|value| {
                let span = input.span_from(start);
                Spanned { span, value }
            })
    }
}

#[cfg(test)]
mod test {

    use combine::eof;
    use combine::error::StringStreamError;
    use combine::{Parser};
    use combine::stream::{state, PointerOffset};
    use combine::easy::{Stream, Errors};

    use crate::span::{Span, SimpleContext};
    use crate::ast::{Literal, TypeName, Integer, Radix};

    use super::{ws, comment, ml_comment, string, ident, type_name, node};
    use super::{literal, number};
    use super::{SpanParser, SpanState};


    fn parse<'x, P>(p: P, text: &'x str)
        -> Result<P::Output, Errors<char, &str, usize>>
        where P: Parser<state::Stream<Stream<&'x str>,
                                      SpanState<'x, SimpleContext>>>
    {
        p.skip(eof()).parse(state::Stream {
            stream: Stream(text),
            state: SpanState {
                span_context: SimpleContext,
                data: text,
            },
        })
        .map(|(val, input)| {
            assert_eq!(input.stream.0, "");
            val
        })
        .map_err(|e| e.map_position(|p| p.translate_position(text)))
    }

    #[test]
    fn parse_ws() {
        parse(ws(), "   ").unwrap();
        parse(ws(), "text").unwrap_err();
    }

    #[test]
    fn parse_comments() {
        parse(comment(), "//hello").unwrap();
        parse(ml_comment(), "/*nothing*/").unwrap();
        parse(ml_comment(), "/*nothing**/").unwrap();
        parse(ml_comment(), "/*no*thing*/").unwrap();
        parse(ml_comment(), "/*no/**/thing*/").unwrap();
        parse(ml_comment(), "/*no/*/**/*/thing*/").unwrap();
        parse((ws(), comment()), "   // hello").unwrap();
        parse((ws(), comment(), ws(), comment()),
              "   // hello\n   //world").unwrap();
    }

    #[test]
    fn parse_comment_err() {
        let err = parse(ws(), r#"/* comment *"#).unwrap_err();
        println!("{}", err);
        assert!(err.to_string().contains("Expected `*/`"));
        assert!(err.to_string().contains("unclosed comment"));

        let err = parse(ws(), r#"/* comment"#).unwrap_err();
        println!("{}", err);
        assert!(err.to_string().contains("Expected `*/`"));
        assert!(err.to_string().contains("unclosed comment"));

        let err = parse(ws(), r#"/*/"#).unwrap_err();
        println!("{}", err);
        assert!(err.to_string().contains("Expected `*/`"));
        assert!(err.to_string().contains("unclosed comment"));

        let err = parse(ws(), r#"xxx"#).unwrap_err();
        println!("{}", err);
        // nothing is expected for comment or whitespace
        assert!(!err.to_string().contains("Expected"));
        assert!(err.to_string().contains("Unexpected `x`"));
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
        assert_eq!(&*parse(string(), r####"r###"a\nb"###"####).unwrap(),
                   "a\\nb");
    }

    #[test]
    fn parse_str_err() {
        let err = parse(string(), r#""hello"#).unwrap_err();
        println!("{}", err);
        assert!(err.to_string().contains("Expected `\"`"));
        assert!(err.to_string().contains("unclosed quoted string"));

        let err = parse(string(), r#""he\u{FFFFFF}llo""#).unwrap_err();
        println!("{}", err);
        assert!(err.to_string().contains("unicode escape out of range"));

        let err = parse(string(), r#""he\u{1234567}llo""#).unwrap_err();
        println!("{}", err);
        assert!(err.to_string().contains("unicode escape"));

        let err = parse(string(), r#""he\u{1gh}llo""#).unwrap_err();
        println!("{}", err);
        assert!(err.to_string().contains("hexadecimal"));

        let err = parse(string(), r#""he\x01llo""#).unwrap_err();
        println!("{}", err);
        assert_eq!(err.position, 4);
        assert!(err.to_string().contains("invalid escape char"));
    }

    #[test]
    fn parse_raw_str_err() {
        let err = parse(string(), r#"r"hello"#).unwrap_err();
        println!("{}", err);
        assert!(err.to_string().contains("Expected `\"`"));
        assert!(err.to_string().contains("unclosed raw string"));

        let err = parse(string(), r###"r#"hello""###).unwrap_err();
        println!("{}", err);
        assert!(err.to_string().contains("Expected `\"#`"));
        assert!(err.to_string().contains("unclosed raw string"));

        let err = parse(string(), r####"r###"hello"####).unwrap_err();
        println!("{}", err);
        assert!(err.to_string().contains("Expected `\"###`"));
        assert!(err.to_string().contains("unclosed raw string"));

        let err = parse(string(), r####"r###"hello"#"####).unwrap_err();
        println!("{}", err);
        assert!(err.to_string().contains("Expected `\"###`"));
        assert!(err.to_string().contains("unclosed raw string"));
    }

    #[test]
    fn parse_spanned() {
        let val = parse(ws().with(SpanParser(string())), r#"   "hello""#)
            .unwrap();
        assert_eq!(*val, "hello".into());
        assert_eq!(val.span(), &Span(3, 10));
    }

    #[test]
    fn parse_ident() {
        assert_eq!(&*parse(ident(), "abcdef").unwrap(), "abcdef");
        assert_eq!(&*parse(ident(), "xx_cd$yy").unwrap(), "xx_cd$yy");
        assert_eq!(&*parse(ident().skip(ws()), "adef   ").unwrap(), "adef");
        assert_eq!(&*parse(ident().skip(ws()), "a123@   ").unwrap(), "a123@");
        parse(ident(), "1abc").unwrap_err();
    }

    #[test]
    fn parse_literal() {
        assert_eq!(parse(literal(), "true").unwrap(), Literal::Bool(true));
        assert_eq!(parse(literal(), "false").unwrap(), Literal::Bool(false));
        assert_eq!(parse(literal(), "null").unwrap(), Literal::Null);
    }

    #[test]
    fn exclude_keywords() {
        parse(node(), "item true").unwrap();

        let err = parse(node(), "true \"item\"").unwrap_err();
        println!("{}", err);
        println!("{:?}", err);
        assert!(err.to_string().contains("cannot be used as a node name"));
        assert!(!err.to_string().contains("Unexpected"));

        let err = parse(node(), "item false=true").unwrap_err();
        println!("{}", err);
        println!("{:?}", err);
        assert!(err.to_string().contains("cannot be used as property names"));
        assert!(!err.to_string().contains("Unexpected"));
    }

    #[test]
    fn parse_type() {
        assert_eq!(parse(type_name(), "(abcdef)").unwrap(),
                   TypeName::from_string("abcdef".into()));
        assert_eq!(parse(type_name(), "(xx_cd$yy)").unwrap(),
                   TypeName::from_string("xx_cd$yy".into()));
        parse(type_name(), "(1abc)").unwrap_err();
        parse(type_name(), "( abc)").unwrap_err();
        parse(type_name(), "(abc )").unwrap_err();
    }

    #[test]
    fn parse_node() {
        let nval = parse(node(), "hello").unwrap();
        assert_eq!(nval.node_name.as_ref(), "hello");
        assert_eq!(nval.type_name.as_ref(), None);

        let nval = parse(node(), "\"123\"").unwrap();
        assert_eq!(nval.node_name.as_ref(), "123");
        assert_eq!(nval.type_name.as_ref(), None);

        let nval = parse(node(), "(typ)other").unwrap();
        assert_eq!(nval.node_name.as_ref(), "other");
        assert_eq!(nval.type_name.as_ref().map(|x| &***x), Some("typ"));

        let nval = parse(node(), "(\"std::duration\")\"timeout\"").unwrap();
        assert_eq!(nval.node_name.as_ref(), "timeout");
        assert_eq!(nval.type_name.as_ref().map(|x| &***x),
                   Some("std::duration"));

        let nval = parse(node(), "hello \"arg1\"").unwrap();
        assert_eq!(nval.node_name.as_ref(), "hello");
        assert_eq!(nval.type_name.as_ref(), None);
        assert_eq!(nval.arguments.len(), 1);
        assert_eq!(nval.properties.len(), 0);
        assert_eq!(&*nval.arguments[0].literal,
                   &Literal::String("arg1".into()));

        let nval = parse(node(), "hello (string)\"arg1\"").unwrap();
        assert_eq!(nval.node_name.as_ref(), "hello");
        assert_eq!(nval.type_name.as_ref(), None);
        assert_eq!(nval.arguments.len(), 1);
        assert_eq!(nval.properties.len(), 0);
        assert_eq!(&***nval.arguments[0].type_name.as_ref().unwrap(),
                   "string");
        assert_eq!(&*nval.arguments[0].literal,
                   &Literal::String("arg1".into()));

        let nval = parse(node(), "hello key=(string)\"arg1\"").unwrap();
        assert_eq!(nval.node_name.as_ref(), "hello");
        assert_eq!(nval.type_name.as_ref(), None);
        assert_eq!(nval.arguments.len(), 0);
        assert_eq!(nval.properties.len(), 1);
        assert_eq!(&***nval.properties.get("key").unwrap()
                   .type_name.as_ref().unwrap(),
                   "string");
        assert_eq!(&*nval.properties.get("key").unwrap().literal,
                   &Literal::String("arg1".into()));

        let nval = parse(node(), "hello key=\"arg1\"").unwrap();
        assert_eq!(nval.node_name.as_ref(), "hello");
        assert_eq!(nval.type_name.as_ref(), None);
        assert_eq!(nval.arguments.len(), 0);
        assert_eq!(nval.properties.len(), 1);
        assert_eq!(&*nval.properties.get("key").unwrap().literal,
                   &Literal::String("arg1".into()));

        let nval = parse(node(), "parent {\nchild\n}").unwrap();
        assert_eq!(nval.node_name.as_ref(), "parent");
        assert_eq!(nval.children().len(), 1);
        assert_eq!(nval.children.as_ref().unwrap()[0].node_name.as_ref(),
                   "child");

        let nval = parse(node(), "parent {\nchild1\nchild2\n}").unwrap();
        assert_eq!(nval.node_name.as_ref(), "parent");
        assert_eq!(nval.children().len(), 2);
        assert_eq!(nval.children.as_ref().unwrap()[0].node_name.as_ref(),
                   "child1");
        assert_eq!(nval.children.as_ref().unwrap()[1].node_name.as_ref(),
                   "child2");
    }

    #[test]
    fn parse_node_err() {
        let err = parse(node(), "hello{").unwrap_err();
        println!("{}", err);
        println!("{:?}", err);
        assert!(err.to_string().contains("Expected `}`"));
        assert!(err.to_string().contains("unclosed block"));

        let err = parse(node(), "hello world").unwrap_err();
        println!("{}", err);
        println!("{:?}", err);
        assert!(err.to_string().contains("cannot be used as argument"));

        let err = parse(node(), "hello world {").unwrap_err();
        println!("{}", err);
        println!("{:?}", err);
        assert!(err.to_string().contains("cannot be used as argument"));
    }

    #[test]
    fn parse_radix_number() {
        assert_eq!(parse(number(), "0x12").unwrap(),
                   Literal::Int(Integer(Radix::Hex, "12".into())));
        assert_eq!(parse(number(), "0xab_12").unwrap(),
                   Literal::Int(Integer(Radix::Hex, "ab12".into())));
        assert_eq!(parse(number(), "0o17").unwrap(),
                   Literal::Int(Integer(Radix::Oct, "17".into())));
        assert_eq!(parse(number(), "0b1010_101").unwrap(),
                   Literal::Int(Integer(Radix::Bin, "1010101".into())));
    }
}
