use std::any::{Any, TypeId};
use std::collections::HashMap;
use std::default::Default;
use std::fmt;

use crate::ast::{Literal, BuiltinType, Value};
use crate::errors::{DecodeError, ExpectedType};
use crate::traits::ErrorSpan;


pub struct Context<S: ErrorSpan> {
    errors: Vec<DecodeError<S>>,
    extensions: HashMap<TypeId, Box<dyn Any>>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Kind {
    Int,
    Decimal,
    String,
    Bool,
    Null,
}

pub fn bytes<S: ErrorSpan>(value: &Value<S>, ctx: &mut Context<S>) -> Vec<u8> {
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

impl<S: ErrorSpan> Context<S> {
    pub(crate) fn new() -> Context<S> {
        Context {
            errors: Vec::new(),
            extensions: HashMap::new(),
        }
    }
    pub fn emit_error(&mut self, err: impl Into<DecodeError<S>>) {
        self.errors.push(err.into());
    }
    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }
    pub(crate) fn into_errors(self) -> Vec<DecodeError<S>> {
        self.errors
    }
    pub fn set<T: 'static>(&mut self, value: T) {
        self.extensions.insert(TypeId::of::<T>(), Box::new(value));
    }
    pub fn get<T: 'static>(&self) -> Option<&T> {
        self.extensions.get(&TypeId::of::<T>())
            .and_then(|b| b.downcast_ref())
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
