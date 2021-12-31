use crate::ast::{Literal, BuiltinType, Value};
use crate::errors::{Error, ResultExt};
use crate::traits::Span;


pub fn bytes<S: Span>(value: &Value<S>) -> Result<Vec<u8>, Error<S>> {
    if let Some(typ) = &value.type_name {
        match typ.as_builtin() {
            Some(&BuiltinType::Base64) => {
                #[cfg(feature="base64")] {
                    match &*value.literal {
                        Literal::String(s) => {
                            base64::decode(s.as_bytes())
                                .err_span(value.literal.span())
                        }
                        _ => Err(Error::new(value.literal.span(),
                                            "expected string value")),
                    }
                }
                #[cfg(not(feature="base64"))] {
                    Err(Error::new(typ.span(),
                        "base64 support is not compiled in"))
                }
            }
            _ => {
                Err(Error::new(typ.span(),
                    "unknown type for bytes, expected `base64` or no type"
                ))
            }
        }
    } else {
        match &*value.literal {
            Literal::String(s) => Ok(s.as_bytes().to_vec()),
            _ => Err(Error::new(value.literal.span(),
                                "expected string value")),
        }
    }
}
