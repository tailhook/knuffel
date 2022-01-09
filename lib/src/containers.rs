use std::sync::Arc;
use std::rc::Rc;

use crate::ast::{SpannedNode, Literal, Value, TypeName};
use crate::decode::Context;
use crate::errors::DecodeError;
use crate::span::Spanned;
use crate::traits::{Span, Decode, DecodeChildren, DecodeScalar, DecodePartial};


impl<S: Span, T: Decode<S>> Decode<S> for Box<T> {
    fn decode_node(node: &SpannedNode<S>, ctx: &mut Context)
        -> Result<Self, DecodeError<S>>
    {
        Decode::decode_node(node, ctx).map(Box::new)
    }
}

impl<S: Span, T: DecodeChildren<S>> DecodeChildren<S> for Box<T> {
    fn decode_children(nodes: &[SpannedNode<S>], ctx: &mut Context)
        -> Result<Self, DecodeError<S>>
    {
        DecodeChildren::decode_children(nodes, ctx).map(Box::new)
    }
}

impl<S: Span, T: DecodePartial<S>> DecodePartial<S> for Box<T> {
    fn insert_child(&mut self, node: &SpannedNode<S>, ctx: &mut Context)
        -> Result<bool, DecodeError<S>>
    {
        (**self).insert_child(node, ctx)
    }
    fn insert_property(&mut self,
                       name: &Spanned<Box<str>, S>, value: &Value<S>,
                       ctx: &mut Context)
        -> Result<bool, DecodeError<S>>
    {
        (**self).insert_property(name, value, ctx)
    }
}

impl<S: Span, T: DecodeScalar<S>> DecodeScalar<S> for Box<T> {
    fn type_check(type_name: &Option<Spanned<TypeName, S>>, ctx: &mut Context) {
        T::type_check(type_name, ctx)
    }
    fn raw_decode(value: &Spanned<Literal, S>, ctx: &mut Context)
        -> Result<Self, DecodeError<S>>
    {
        DecodeScalar::raw_decode(value, ctx).map(Box::new)
    }
}

impl<S: Span, T: Decode<S>> Decode<S> for Arc<T> {
    fn decode_node(node: &SpannedNode<S>, ctx: &mut Context)
        -> Result<Self, DecodeError<S>>
    {
        Decode::decode_node(node, ctx).map(Arc::new)
    }
}

impl<S: Span, T: DecodeChildren<S>> DecodeChildren<S> for Arc<T> {
    fn decode_children(nodes: &[SpannedNode<S>], ctx: &mut Context)
        -> Result<Self, DecodeError<S>>
    {
        DecodeChildren::decode_children(nodes, ctx).map(Arc::new)
    }
}

impl<S: Span, T: DecodePartial<S>> DecodePartial<S> for Arc<T> {
    fn insert_child(&mut self, node: &SpannedNode<S>, ctx: &mut Context)
        -> Result<bool, DecodeError<S>>
    {
        Arc::get_mut(self).expect("no Arc clone yet")
            .insert_child(node, ctx)
    }
    fn insert_property(&mut self,
                       name: &Spanned<Box<str>, S>, value: &Value<S>,
                       ctx: &mut Context)
        -> Result<bool, DecodeError<S>>
    {
        Arc::get_mut(self).expect("no Arc clone yet")
            .insert_property(name, value, ctx)
    }
}

impl<S: Span, T: DecodeScalar<S>> DecodeScalar<S> for Arc<T> {
    fn type_check(type_name: &Option<Spanned<TypeName, S>>, ctx: &mut Context)
    {
        T::type_check(type_name, ctx)
    }
    fn raw_decode(value: &Spanned<Literal, S>, ctx: &mut Context)
        -> Result<Self, DecodeError<S>>
    {
        DecodeScalar::raw_decode(value, ctx).map(Arc::new)
    }
}

impl<S: Span, T: Decode<S>> Decode<S> for Rc<T> {
    fn decode_node(node: &SpannedNode<S>, ctx: &mut Context)
        -> Result<Self, DecodeError<S>>
    {
        Decode::decode_node(node, ctx).map(Rc::new)
    }
}

impl<S: Span, T: DecodeChildren<S>> DecodeChildren<S> for Rc<T> {
    fn decode_children(nodes: &[SpannedNode<S>], ctx: &mut Context)
        -> Result<Self, DecodeError<S>>
    {
        DecodeChildren::decode_children(nodes, ctx).map(Rc::new)
    }
}

impl<S: Span, T: DecodePartial<S>> DecodePartial<S> for Rc<T> {
    fn insert_child(&mut self, node: &SpannedNode<S>, ctx: &mut Context)
        -> Result<bool, DecodeError<S>>
    {
        Rc::get_mut(self).expect("no Rc clone yet")
            .insert_child(node, ctx)
    }
    fn insert_property(&mut self,
                       name: &Spanned<Box<str>, S>, value: &Value<S>,
                       ctx: &mut Context)
        -> Result<bool, DecodeError<S>>
    {
        Rc::get_mut(self).expect("no Rc clone yet")
            .insert_property(name, value, ctx)
    }
}

impl<S: Span, T: DecodeScalar<S>> DecodeScalar<S> for Rc<T> {
    fn type_check(type_name: &Option<Spanned<TypeName, S>>, ctx: &mut Context)
    {
        T::type_check(type_name, ctx)
    }
    fn raw_decode(value: &Spanned<Literal, S>, ctx: &mut Context)
        -> Result<Self, DecodeError<S>>
    {
        DecodeScalar::raw_decode(value, ctx).map(Rc::new)
    }
}
