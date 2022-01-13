use chumsky::Parser;

use crate::ast::Document;
use crate::decode::Context;
use crate::errors::{Error, AddSource, KdlSource, SyntaxErrors, DecodeErrors};
use crate::grammar;
use crate::span::{Span};
use crate::traits::{self, DecodeChildren};


pub fn parse_ast<S: traits::Span>(file_name: &str, text: &str)
    -> Result<Document<S>, Error<S>>
{
    grammar::document()
    .parse(S::stream(text))
    .map_err(|errors| {
        let source_code = KdlSource::new(file_name, text.to_string());
        let e = SyntaxErrors {
            errors: errors.into_iter().map(|error| {
                AddSource {
                    source_code: source_code.clone(),
                    error,
                }
            }).collect(),
        };
        Error::Syntax(e)
    })
}

pub fn parse<T>(file_name: &str, text: &str) -> Result<T, Error<Span>>
    where T: DecodeChildren<Span>,
{
    parse_with_context(file_name, text, |_| {})
}

pub fn parse_with_context<T, S, F>(file_name: &str, text: &str, set_ctx: F)
    -> Result<T, Error<S>>
    where F: FnOnce(&mut Context<S>),
          T: DecodeChildren<S>,
          S: traits::Span,
{
    let ast = parse_ast(file_name, text)?;

    let mut ctx = Context::new();
    set_ctx(&mut ctx);
    let errors = match DecodeChildren::decode_children(&ast.nodes, &mut ctx) {
        Ok(_) if ctx.has_errors() => {
            ctx.into_errors()
        }
        Err(e) => {
            ctx.emit_error(e);
            ctx.into_errors()
        }
        Ok(v) => return Ok(v)
    };
    let source_code = KdlSource::new(file_name, text.to_string());
    let e = DecodeErrors {
        errors: errors.into_iter().map(|error| {
            AddSource {
                source_code: source_code.clone(),
                error,
            }
        }).collect(),
    };
    Err(Error::Decode(e))
}

#[test]
fn normal() {
    let doc = parse_ast::<Span>("<embedded.kdl>", r#"node "hello""#).unwrap();
    assert_eq!(doc.nodes.len(), 1);
    assert_eq!(&**doc.nodes[0].node_name, "node");
}
