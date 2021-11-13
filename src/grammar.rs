use combine::error::StreamError;
use combine::stream::StreamErrorFor;
use combine::stream::state;
use combine::parser::combinator::recognize;
use combine::parser::repeat::escaped;
use combine::{Stream, Parser};
use combine::{eof, optional, count_min_max, between, position};
use combine::{token, many, skip_many1, skip_many, satisfy, satisfy_map};

use crate::ast::{Literal};
use crate::span::{Spanned, SpanContext};

pub struct SpanParser<P>(P);

fn ws_char<I: Stream<Token=char>>() -> impl Parser<I, Output=()> {
    satisfy(|c| matches!(c,
        '\t' | ' ' | '\u{00a0}' | '\u{1680}' |
        '\u{2000}'..='\u{200A}' |
        '\u{202F}' | '\u{205F}' | '\u{3000}'
    ))
    .map(|_| ())
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
        'u' => todo!(),
        _ => None,
    })
    .or(token('u').and(token('{')).with(
        count_min_max(1, 6, satisfy_map(|x: char| x.to_digit(16))),
    ).skip(token('}')).and_then(|code: UniEscape| {
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

pub struct SpanState<'a, S> {
    span_context: S,
    data: &'a str,
}

impl<'a, I, S, P> Parser<state::Stream<I, SpanState<'a, S>>> for SpanParser<P>
    where P: Parser<state::Stream<I, SpanState<'a, S>>>,
          S: SpanContext<usize>,
          I: Stream<Position=combine::stream::PointerOffset<str>>,
{
    type Output = Spanned<P::Output, S::Span>;
    type PartialState = ();

    #[inline]
    fn parse_lazy(&mut self, input: &mut state::Stream<I, SpanState<'a, S>>)
        -> combine::ParseResult<Self::Output, I::Error>
    {
        let start = input.stream.position();
        self.0.parse_lazy(input)
            .map(|value| {
                let end = input.stream.position();
                let start = start.translate_position(input.state.data);
                let end = end.translate_position(input.state.data);
                let span = input.state.span_context.from_positions(start, end);
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
    use crate::ast::Literal;

    use super::{ws, string, SpanParser, SpanState};


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
    }

    #[test]
    fn parse_str_err() {
        let err = parse(string(), r#""hello"#).unwrap_err();
        println!("{}", err);
        assert!(err.to_string().contains("Expected `\"`"));
    }

    #[test]
    fn parse_spanned() {
        let val = parse(ws().with(SpanParser(string())), r#"   "hello""#)
            .unwrap();
        assert_eq!(*val, Literal::String("hello".into()));
        assert_eq!(val.span(), &Span(3, 10));
    }

}
