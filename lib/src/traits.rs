use std::fmt;

use crate::ast::{SpannedNode, Literal, Value, TypeName};
use crate::span::Spanned;
use crate::errors::DecodeError;
use crate::decode::Context;


pub trait Decode<S: ErrorSpan>: Sized {
    fn decode_node(node: &SpannedNode<S>, ctx: &mut Context<S>)
        -> Result<Self, DecodeError<S>>;
}

pub trait DecodeChildren<S: ErrorSpan>: Sized {
    fn decode_children(nodes: &[SpannedNode<S>], ctx: &mut Context<S>)
        -> Result<Self, DecodeError<S>>;
}

pub trait DecodePartial<S: ErrorSpan>: Sized {
    fn insert_child(&mut self, node: &SpannedNode<S>, ctx: &mut Context<S>)
        -> Result<bool, DecodeError<S>>;
    fn insert_property(&mut self,
                       name: &Spanned<Box<str>, S>, value: &Value<S>,
                       ctx: &mut Context<S>)
        -> Result<bool, DecodeError<S>>;
}

pub trait DecodeScalar<S: ErrorSpan>: Sized {
    fn type_check(type_name: &Option<Spanned<TypeName, S>>,
                  ctx: &mut Context<S>);
    fn raw_decode(value: &Spanned<Literal, S>, ctx: &mut Context<S>)
        -> Result<Self, DecodeError<S>>;
    fn decode(value: &Value<S>, ctx: &mut Context<S>)
        -> Result<Self, DecodeError<S>>
    {
        Self::type_check(&value.type_name, ctx);
        Self::raw_decode(&value.literal, ctx)
    }
}


pub trait DecodeSpan<S: ErrorSpan>: Sized {
    fn decode_span(span: S, ctx: &mut Context<S>) -> Self;
}

impl<T: ErrorSpan> DecodeSpan<T> for T {
    fn decode_span(span: T, _: &mut Context<T>) -> Self {
        span
    }
}

pub trait ErrorSpan: Into<miette::SourceSpan>
                     + Clone + fmt::Debug + Send + Sync + 'static {}
impl<T> ErrorSpan for T
    where T: Into<miette::SourceSpan>,
          T: Clone + fmt::Debug + Send + Sync + 'static,
{}


pub trait Span: parsing_span::Sealed + chumsky::Span + ErrorSpan {}

pub(crate) mod parsing_span {
    pub trait Sealed {
        /// Note assuming ascii, single-width, non-newline chars here
        fn at_start(&self, chars: usize) -> Self;
        fn at_end(&self) -> Self;
        /// Note assuming ascii, single-width, non-newline chars here
        fn before_start(&self, chars: usize) -> Self;
        fn length(&self) -> usize;

        fn stream(s: &str) -> chumsky::Stream<'_, char, Self, std::iter::Map<std::str::CharIndices<'_>, fn((usize, char)) -> (char, Self)>>
            where Self: chumsky::Span;
    }
}
