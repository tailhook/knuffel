use std::fmt;
use std::default::Default;

use miette::Diagnostic;

use crate::ast::{Literal, BuiltinType, Value};
use crate::errors::{DecodeError, ExpectedType};
use crate::traits::Span;

pub struct Context {
    errors: Vec<Box<dyn Diagnostic + Send + Sync + 'static>>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Kind {
    Int,
    Decimal,
    String,
    Bool,
    Null,
}

pub fn bytes<S: Span>(value: &Value<S>, ctx: &mut Context) -> Vec<u8> {
    if let Some(typ) = &value.type_name {
        match typ.as_builtin() {
            Some(&BuiltinType::Base64) => {
                #[cfg(feature="base64")] {
                    match &*value.literal {
                        Literal::String(s) => {
                            match base64::decode(s.as_bytes()) {
                                Ok(vec) => vec,
                                Err(e) => {
                                    ctx.emit_error(DecodeError::conversion(
                                        &value.literal, e));
                                    Default::default()
                                }
                            }
                        }
                        _ => {
                            ctx.emit_error(DecodeError::scalar_kind(
                                Kind::String, &value.literal));
                            Default::default()
                        }
                    }
                }
                #[cfg(not(feature="base64"))] {
                    ctx.emit_error(DecodeError::unsupported(
                            value, "base64 support is not compiled in"));
                    Default::default()
                }
            }
            _ => {
                ctx.emit_error(DecodeError::TypeName {
                    span: typ.span().clone(),
                    found: Some(typ.value.clone()),
                    expected: ExpectedType::optional(BuiltinType::Base64),
                    rust_type: "bytes",
                });
                Default::default()
            }
        }
    } else {
        match &*value.literal {
            Literal::String(s) => s.as_bytes().to_vec(),
            _ => {
                ctx.emit_error(DecodeError::scalar_kind(
                    Kind::String, &value.literal));
                Default::default()
            }
        }
    }
}

impl Context {
    pub(crate) fn new() -> Context {
        Context {
            errors: Vec::new(),
        }
    }
    pub fn emit_error<S: Span>(&mut self, err: impl Into<DecodeError<S>>) {
        self.errors.push(Box::new(err.into()));
    }
}

impl fmt::Display for Kind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

impl From<&'_ Literal> for Kind {
    fn from(lit: &Literal) -> Kind {
        use Literal as L;
        use Kind as K;
        match lit {
            L::Int(_) => K::Int,
            L::Decimal(_) => K::Decimal,
            L::String(_) => K::String,
            L::Bool(_) => K::Bool,
            L::Null => K::Null,
        }
    }
}

impl Kind {
    pub const fn as_str(&self) -> &'static str {
        use Kind::*;
        match self {
            Int => "integer",
            Decimal => "decimal",
            String => "string",
            Bool => "boolean",
            Null => "null",
        }
    }
}
