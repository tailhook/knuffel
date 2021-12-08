use crate::ast::{SpannedNode, Literal, Value};
use crate::span::Spanned;
use crate::errors::Error;


pub trait Decode<S>: Sized {
    fn decode_node(node: &SpannedNode<S>) -> Result<Self, Error<S>>;
    fn decode_children(nodes: &[SpannedNode<S>]) -> Result<Self, Error<S>>;
}

pub trait DecodeScalar<S>: Sized {
    fn decode(value: &Spanned<Literal, S>) -> Result<Self, Error<S>>;
}

pub trait DecodeTypedScalar<S>: Sized {
    fn decode(value: &Value<S>) -> Result<Self, Error<S>>;
}

pub trait Span: Clone {
}
