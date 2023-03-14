use std::str::FromStr;
use std::path::PathBuf;
use std::default::Default;

use crate::ast::{Literal, Integer, Decimal, Radix, TypeName, BuiltinType};
use crate::decode::{Context, Kind};
use crate::errors::{DecodeError, ExpectedType};
use crate::span::{Spanned};
use crate::traits::{ErrorSpan, DecodeScalar};


macro_rules! impl_integer {
    ($typ: ident, $marker: ident) => {
        impl TryFrom<&Integer> for $typ {
            type Error = <$typ as FromStr>::Err;
            fn try_from(val: &Integer) -> Result<$typ, <$typ as FromStr>::Err>
            {
                match val.0 {
                    Radix::Bin => <$typ>::from_str_radix(&val.1, 2),
                    Radix::Oct => <$typ>::from_str_radix(&val.1, 8),
                    Radix::Dec => <$typ>::from_str(&val.1),
                    Radix::Hex => <$typ>::from_str_radix(&val.1, 16),
                }
            }
        }

        impl<S: ErrorSpan> DecodeScalar<S> for $typ {
            fn raw_decode(val: &Spanned<Literal, S>, ctx: &mut Context<S>)
                -> Result<$typ, DecodeError<S>>
            {
                match &**val {
                    Literal::Int(ref value) => {
                        match value.try_into() {
                            Ok(val) => Ok(val),
                            Err(e) => {
                                ctx.emit_error(DecodeError::conversion(val, e));
                                Ok(0)
                            }
                        }
                    }
                    _ => {
                        ctx.emit_error(DecodeError::scalar_kind(
                                Kind::String, val));
                        Ok(0)
                    }
                }
            }
            fn type_check(type_name: &Option<Spanned<TypeName, S>>,
                          ctx: &mut Context<S>)
            {
                if let Some(typ) = type_name {
                    if typ.as_builtin() != Some(&BuiltinType::$marker) {
                        ctx.emit_error(DecodeError::TypeName {
                            span: typ.span().clone(),
                            found: Some(typ.value.clone()),
                            expected: ExpectedType::optional(
                                BuiltinType::$marker),
                            rust_type: stringify!($typ),
                        });
                    }
                }
            }
        }
    }
}

impl_integer!(i8, I8);
impl_integer!(u8, U8);
impl_integer!(i16, I16);
impl_integer!(u16, U16);
impl_integer!(i32, I32);
impl_integer!(u32, U32);
impl_integer!(i64, I64);
impl_integer!(u64, U64);
impl_integer!(isize, Isize);
impl_integer!(usize, Usize);

macro_rules! impl_float {
    ($typ: ident, $marker: ident) => {
        impl TryFrom<&Decimal> for $typ {
            type Error = <$typ as FromStr>::Err;
            fn try_from(val: &Decimal) -> Result<$typ, <$typ as FromStr>::Err>
            {
                <$typ>::from_str(&val.0)
            }
        }

        impl<S: ErrorSpan> DecodeScalar<S> for $typ {
            fn raw_decode(val: &Spanned<Literal, S>, ctx: &mut Context<S>)
                -> Result<$typ, DecodeError<S>>
            {
                match &**val {
                    Literal::Decimal(ref value) => {
                        match value.try_into() {
                            Ok(val) => Ok(val),
                            Err(e) => {
                                ctx.emit_error(DecodeError::conversion(val, e));
                                Ok(0.0)
                            }
                        }
                    }
                    _ => {
                        ctx.emit_error(DecodeError::scalar_kind(
                                Kind::String, val));
                        Ok(0.0)
                    }
                }
            }
            fn type_check(type_name: &Option<Spanned<TypeName, S>>,
                          ctx: &mut Context<S>)
            {
                if let Some(typ) = type_name {
                    if typ.as_builtin() != Some(&BuiltinType::$marker) {
                        ctx.emit_error(DecodeError::TypeName {
                            span: typ.span().clone(),
                            found: Some(typ.value.clone()),
                            expected: ExpectedType::optional(
                                BuiltinType::$marker),
                            rust_type: stringify!($typ),
                        });
                    }
                }
            }
        }
    }
}

impl_float!(f32, F32);
impl_float!(f64, F64);

impl<S: ErrorSpan> DecodeScalar<S> for String {
    fn raw_decode(val: &Spanned<Literal, S>, ctx: &mut Context<S>)
        -> Result<String, DecodeError<S>>
    {
        match &**val {
            Literal::String(ref s) => Ok(s.clone().into()),
            _ => {
                ctx.emit_error(DecodeError::scalar_kind(Kind::String, val));
                Ok(String::new())
            }
        }
    }
    fn type_check(type_name: &Option<Spanned<TypeName, S>>,
                  ctx: &mut Context<S>)
    {
        if let Some(typ) = type_name {
            ctx.emit_error(DecodeError::TypeName {
                span: typ.span().clone(),
                found: Some(typ.value.clone()),
                expected: ExpectedType::no_type(),
                rust_type: "String",
            });
        }
    }
}


impl<S: ErrorSpan> DecodeScalar<S> for PathBuf {
    fn raw_decode(val: &Spanned<Literal, S>, ctx: &mut Context<S>)
        -> Result<PathBuf, DecodeError<S>>
    {
        match &**val {
            Literal::String(ref s) => Ok(String::from(s.clone()).into()),
            _ => {
                ctx.emit_error(DecodeError::scalar_kind(Kind::String, val));
                Ok(Default::default())
            }
        }
    }
    fn type_check(type_name: &Option<Spanned<TypeName, S>>,
                  ctx: &mut Context<S>)
    {
        if let Some(typ) = type_name {
            ctx.emit_error(DecodeError::TypeName {
                span: typ.span().clone(),
                found: Some(typ.value.clone()),
                expected: ExpectedType::no_type(),
                rust_type: "PathBuf",
            });
        }
    }
}

impl<S: ErrorSpan> DecodeScalar<S> for bool {
    fn raw_decode(val: &Spanned<Literal, S>, ctx: &mut Context<S>)
        -> Result<bool, DecodeError<S>>
    {
        match &**val {
            Literal::Bool(value) => Ok(*value),
            _ => {
                ctx.emit_error(DecodeError::scalar_kind(Kind::Bool, &val));
                Ok(Default::default())
            }
        }
    }
    fn type_check(type_name: &Option<Spanned<TypeName, S>>,
                  ctx: &mut Context<S>)
    {
        if let Some(typ) = type_name {
            ctx.emit_error(DecodeError::TypeName {
                span: typ.span().clone(),
                found: Some(typ.value.clone()),
                expected: ExpectedType::no_type(),
                rust_type: "bool",
            });
        }
    }
}
