use std::fmt;

use crate::ast::{SpannedNode, Literal, Value, TypeName};
use crate::span::Spanned;
use crate::errors::Error;


pub trait Decode<S>: Sized {
    fn decode_node(node: &SpannedNode<S>) -> Result<Self, Error<S>>;
}

pub trait DecodeChildren<S>: Sized {
    fn decode_children(nodes: &[SpannedNode<S>]) -> Result<Self, Error<S>>;
}

pub trait DecodePartial<S>: Sized {
    fn insert_child(&mut self, node: &SpannedNode<S>) -> Result<bool, Error<S>>;
    fn insert_property(&mut self, name: &Spanned<Box<str>, S>, value: &Value<S>)
        -> Result<bool, Error<S>>;
}

pub trait DecodeScalar<S>: Sized {
    fn type_check(type_name: &Option<Spanned<TypeName, S>>)
        -> Result<(), Error<S>>;
    fn raw_decode(value: &Spanned<Literal, S>) -> Result<Self, Error<S>>;
    fn decode(value: &Value<S>) -> Result<Self, Error<S>> {
        Self::type_check(&value.type_name)?;
        Self::raw_decode(&value.literal)
    }
}

pub trait Span: Into<miette::SourceSpan> + chumsky::Span<Offset=usize>
                + fmt::Debug + Clone + 'static
{
    fn length(&self) -> usize {
        self.end() - self.start()
    }
}
