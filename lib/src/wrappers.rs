use chumsky::{Parser, Stream};

use crate::ast::Document;
use crate::errors::{RealError, ParseError, AddSource};
use crate::grammar;
use crate::span::Span;


pub fn raw_parse(text: &str) -> Result<Document<Span>, RealError> {
    grammar::document()
    .parse(Stream::from_iter(
        Span(text.len(), text.len()),
        text.char_indices()
            .map(|(i, c)| (c, Span(i, i + c.len_utf8()))),
    ))
    .map_err(|errors| {
        let source: std::sync::Arc<String> = text.to_string().into();
        let e = ParseError {
            errors: errors.into_iter().map(|error| {
                AddSource {
                    source: source.clone(),
                    error,
                }
            }).collect(),
        };
        RealError::Parse(e)
    })
}

#[test]
fn normal() {
    let doc = raw_parse(r#"node "hello""#).unwrap();
    assert_eq!(doc.nodes.len(), 1);
    assert_eq!(&**doc.nodes[0].node_name, "node");
}
