use std::fmt;

use crate::ast::{SpannedNode, Literal, Value, TypeName};
use crate::span::Spanned;
use crate::errors::DecodeError;
use crate::decode::Context;


pub trait Decode<S: Span>: Sized {
    fn decode_node(node: &SpannedNode<S>, ctx: &mut Context)
        -> Result<Self, DecodeError<S>>;
}

pub trait DecodeChildren<S: Span>: Sized {
    fn decode_children(nodes: &[SpannedNode<S>], ctx: &mut Context)
        -> Result<Self, DecodeError<S>>;
}

pub trait DecodePartial<S: Span>: Sized {
    fn insert_child(&mut self, node: &SpannedNode<S>, ctx: &mut Context)
        -> Result<bool, DecodeError<S>>;
    fn insert_property(&mut self,
                       name: &Spanned<Box<str>, S>, value: &Value<S>,
                       ctx: &mut Context)
        -> Result<bool, DecodeError<S>>;
}

pub trait DecodeScalar<S: Span>: Sized {
    fn type_check(type_name: &Option<Spanned<TypeName, S>>, ctx: &mut Context);
    fn raw_decode(value: &Spanned<Literal, S>, ctx: &mut Context)
        -> Result<Self, DecodeError<S>>;
    fn decode(value: &Value<S>, ctx: &mut Context)
        -> Result<Self, DecodeError<S>>
    {
        Self::type_check(&value.type_name, ctx);
        Self::raw_decode(&value.literal, ctx)
    }
}

pub trait Span: Into<miette::SourceSpan> + chumsky::Span<Offset=usize>
                + fmt::Debug + Clone + Send + Sync + 'static
{
    fn length(&self) -> usize {
        self.end() - self.start()
    }
}
