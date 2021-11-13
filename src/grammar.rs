use std::collections::BTreeMap;

use combine::error::StreamError;
use combine::parser::combinator::recognize;
use combine::parser::repeat::escaped;
use combine::stream::state;
use combine::stream::{PointerOffset, StreamErrorFor};
use combine::{Stream, Parser};
use combine::{eof, optional, count_min_max, between, position};
use combine::{token, many, skip_many1, skip_many, satisfy, satisfy_map};

use crate::ast::{Literal, TypeName, Node};
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

fn ws_char<I: Stream<Token=char>>() -> impl Parser<I, Output=()> {
    satisfy(|c| matches!(c,
        '\t' | ' ' | '\u{00a0}' | '\u{1680}' |
        '\u{2000}'..='\u{200A}' |
        '\u{202F}' | '\u{205F}' | '\u{3000}'
    ))
    .map(|_| ())
}

fn first_id_char<I: Stream<Token=char>>() -> impl Parser<I, Output=char> {
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
    (token('\r'), optional(token('\n'))).map(|_| ())
    .or(satisfy(|c| matches!(c,
        '\n' | '\u{0085}' | '\u{000C}' | '\u{2028}' | '\u{2029}'))
        .map(|_| ()))
}

fn not_nl_char<I: Stream<Token=char>>() -> impl Parser<I, Output=()> {
    satisfy(|c| !matches!(c,
        '\r' | '\n' | '\u{0085}' | '\u{000C}' | '\u{2028}' | '\u{2029}'))
    .map(|_| ())
}

fn ws<I: Stream<Token=char>>() -> impl Parser<I, Output=()> {
    let comment =
        (token('/'), token('/'), skip_many(not_nl_char()), newline().or(eof()))
        .map(|_| ());
    // Wrap the `spaces().or(comment)` in `skip_many` so that it skips alternating whitespace and
    // comments
    skip_many(skip_many1(ws_char()).or(comment))
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
                StreamErrorFor::<I>::unexpected_static_message(
                    "unicode escape out of range")
            })
        }))
}


fn string<I: Stream<Token=char>>() -> impl Parser<I, Output=Literal> {
    between(token('"'), token('"'), many(
        satisfy(|c| c != '"' && c != '\\')
        .or(token('\\').with(esc_value()))
        .silent()  // expose only unexpected '"'
    ))
    .map(|val: String| Literal::String(val.into()))
}

fn ident<I: Stream<Token=char>>() -> impl Parser<I, Output=Box<str>> {
    first_id_char().and(many(id_char()))
        .map(|(first, mut ident): (_, String)| {
            ident.insert(0, first);
            ident.into()
        })
}

fn type_name<I: Stream<Token=char>>() -> impl Parser<I, Output=TypeName> {
    token('(').with(ident()).skip(token(')')).map(TypeName::from_string)
}

fn node<I>() -> impl Parser<I, Output=Node<I::Span>>
    where I: SpanStream<Token=char>,
{
    combine::struct_parser! {
        Node {
            type_name: optional(SpanParser(type_name())),
            node_name: SpanParser(ident()),
            arguments: combine::produce(Vec::new),
            properties: combine::produce(BTreeMap::new),
            children: combine::produce(|| None),
        }
    }
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
    use crate::ast::{Literal, TypeName};

    use super::{ws, string, ident, type_name, node};
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
        parse(ws(), "//hello").unwrap();
        parse(ws(), "   // hello").unwrap();
        parse(ws(), "   // hello\n   //world").unwrap();
        parse(ws(), "text").unwrap_err();
    }

    #[test]
    fn parse_str() {
        assert_eq!(parse(string(), r#""hello""#).unwrap(),
            Literal::String("hello".into()));
        assert_eq!(parse(string(), r#""""#).unwrap(),
            Literal::String("".into()));
        assert_eq!(parse(string(), r#""hel\"lo""#).unwrap(),
            Literal::String("hel\"lo".into()));
        assert_eq!(parse(string(), r#""hello\nworld!""#).unwrap(),
            Literal::String("hello\nworld!".into()));
        assert_eq!(parse(string(), r#""\u{1F680}""#).unwrap(),
            Literal::String("🚀".into()));
    }

    #[test]
    fn parse_str_err() {
        let err = parse(string(), r#""hello"#).unwrap_err();
        println!("{}", err);
        assert!(err.to_string().contains("Expected `\"`"));

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
    fn parse_spanned() {
        let val = parse(ws().with(SpanParser(string())), r#"   "hello""#)
            .unwrap();
        assert_eq!(*val, Literal::String("hello".into()));
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

        let nval = parse(node(), "(typ)other").unwrap();
        assert_eq!(nval.node_name.as_ref(), "other");
        assert_eq!(nval.type_name.as_ref().map(|x| &***x), Some("typ"));
    }
}
