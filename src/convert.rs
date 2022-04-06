use std::str::FromStr;
use std::path::PathBuf;
use std::default::Default;

use crate::ast::{Literal, Integer, Decimal, Radix, TypeName, BuiltinType};
use crate::decode::{Context, Kind};
use crate::errors::{DecodeError, ExpectedType};
use crate::span::{Spanned};
use crate::traits::{ErrorSpan, DecodeScalar};


macro_rules! impl_number {
    ($(($which: ident, $typ: ident, $marker: ident),)+) => {
        $(
            impl_number!($which, $typ, $marker);
        )*
    };
    (Integer, $typ: ident, $marker: ident) => {
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

        impl_number!(_Impl, Int, $typ, $marker, 0);
    };
    (Decimal, $typ: ident, $marker: ident) => {
        impl TryFrom<&Decimal> for $typ {
            type Error = <$typ as FromStr>::Err;
            fn try_from(val: &Decimal) -> Result<$typ, <$typ as FromStr>::Err>
            {
                <$typ>::from_str(&val.0)
            }
        }

        impl_number!(_Impl, Decimal, $typ, $marker, 0.0);
    };
    (_Impl, $variant: ident, $typ: ident, $marker: ident, $default: expr) => {
        impl<S: ErrorSpan> DecodeScalar<S> for $typ {
            fn raw_decode(val: &Spanned<Literal, S>, ctx: &mut Context<S>)
                -> Result<$typ, DecodeError<S>>
            {
                match &**val {
                    Literal::$variant(ref value) => {
                        match value.try_into() {
                            Ok(val) => Ok(val),
                            Err(e) => {
                                ctx.emit_error(DecodeError::conversion(val, e));
                                Ok($default)
                            }
                        }
                    }
                    _ => {
                        ctx.emit_error(DecodeError::scalar_kind(
                                Kind::String, val));
                        Ok($default)
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
    };
}

impl_number!(
    (Integer, i8, I8),
    (Integer, u8, U8),
    (Integer, i16, I16),
    (Integer, u16, U16),
    (Integer, i32, I32),
    (Integer, u32, U32),
    (Integer, i64, I64),
    (Integer, u64, U64),
    (Integer, isize, Isize),
    (Integer, usize, Usize),
    (Decimal, f32, F32),
    (Decimal, f64, F64),
);

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
