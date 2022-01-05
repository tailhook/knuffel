use combine::Parser;
use combine::easy::Stream;
use combine::stream::state;

use crate::ast::Document;
use crate::errors::RawError;
use crate::errors::{RealError, ParseError, ParseErrorEnum};
use crate::grammar::{self, SpanState};
use crate::span::{Span, SimpleContext};


pub fn raw_parse(text: &str) -> Result<Document<Span>, RawError<Span>> {
    let (doc, _) = grammar::document().parse(state::Stream {
        stream: Stream(text),
        state: SpanState {
            span_context: SimpleContext,
            data: text,
        },
    }).map_err(|error| {
        error
            .map_position(|p| p.translate_position(text))
            .map_position(|p| Span(p, p))
    })?;
    Ok(doc)
}

#[test]
fn normal() {
    let doc = raw_parse(r#"node "hello""#).unwrap();
    assert_eq!(doc.nodes.len(), 1);
    assert_eq!(&**doc.nodes[0].node_name, "node");
}
