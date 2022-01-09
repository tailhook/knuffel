use chumsky::{Parser, Stream};

use crate::ast::Document;
use crate::errors::{Error, ParseError, AddSource};
use crate::grammar;
use crate::span::Span;


pub fn parse_ast(text: &str) -> Result<Document<Span>, Error> {
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
                    source_code: source.clone(),
                    error,
                }
            }).collect(),
        };
        Error::Syntax(Box::new(e))
    })
}

/*
pub fn decode_ast<T, S>(ast: &Document<S>) -> Result<T, Error>
    where T: DecodeChildren<S>,
          S: crate::traits::Span,
{
    let mut ctx = Context::new();
    match DecodeChildren::decode_children(ast, &mut ctx) {
        Ok(v) if ctx.has_errors() {
            Err(ctx.into_error())
        }
        Err(e) => {
            ctx.emit_error(e);
            Err(ctx.into_error())
        }
        Ok(v) => Ok(v)
    }
}
*/

#[test]
fn normal() {
    let doc = raw_parse(r#"node "hello""#).unwrap();
    assert_eq!(doc.nodes.len(), 1);
    assert_eq!(&**doc.nodes[0].node_name, "node");
}
