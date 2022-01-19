//! Traits used for the library
//!
//! Most users will never implement these manually. See
//! [`Decode`](derive@crate::Decode)` and
//! [`DecodeScalar`](derive@crate::DecodeScalar) for a
//! documentation of the derives to implement these traits.
use std::fmt;

use crate::ast::{SpannedNode, Literal, Value, TypeName};
use crate::span::Spanned;
use crate::errors::DecodeError;
use crate::decode::Context;


/// Trait to decode KDL node from the AST
pub trait Decode<S: ErrorSpan>: Sized {
    /// Decodes the node from the ast
    fn decode_node(node: &SpannedNode<S>, ctx: &mut Context<S>)
        -> Result<Self, DecodeError<S>>;
}

/// Trait to decode children of the KDL node, mostly used for root document
pub trait DecodeChildren<S: ErrorSpan>: Sized {
    /// Decodes from a list of chidren ASTs
    fn decode_children(nodes: &[SpannedNode<S>], ctx: &mut Context<S>)
        -> Result<Self, DecodeError<S>>;
}

/// The trait is implemented for structures that can be used as part of other
/// structs
///
/// The type of field that `#[knuffel(flatten)]` is used for should implement
/// this trait. It is automatically implemented by `#[derive(knuffel::Decode)]`
/// by structures that have only optional properties and children (no
/// arguments).
pub trait DecodePartial<S: ErrorSpan>: Sized {
    /// The method is called when unknown child is encountered by parent
    /// structure
    ///
    /// Returns `Ok(true)` if the child is "consumed" (i.e. stored in this
    /// structure).
    fn insert_child(&mut self, node: &SpannedNode<S>, ctx: &mut Context<S>)
        -> Result<bool, DecodeError<S>>;
    /// The method is called when unknown property is encountered by parent
    /// structure
    ///
    /// Returns `Ok(true)` if the property is "consumed" (i.e. stored in this
    /// structure).
    fn insert_property(&mut self,
                       name: &Spanned<Box<str>, S>, value: &Value<S>,
                       ctx: &mut Context<S>)
        -> Result<bool, DecodeError<S>>;
}

/// The trait that decodes scalar value and checks its type
pub trait DecodeScalar<S: ErrorSpan>: Sized {
    /// Typecheck the value
    ///
    /// This method can only emit errors to the context in type mismatch case.
    /// Errors emitted to the context are considered fatal once the whole data
    /// is processed but non fatal when encountered. So even if there is a type
    /// in type name we can proceed and try parsing actual value.
    fn type_check(type_name: &Option<Spanned<TypeName, S>>,
                  ctx: &mut Context<S>);
    /// Decode value without typecheck
    ///
    /// This can be used by wrappers to parse some know value but use a
    /// different typename (kinda emulated subclassing)
    fn raw_decode(value: &Spanned<Literal, S>, ctx: &mut Context<S>)
        -> Result<Self, DecodeError<S>>;
    /// Decode the value and typecheck
    ///
    /// This should not be overriden and uses `type_check` in combination with
    /// `raw_decode`.
    fn decode(value: &Value<S>, ctx: &mut Context<S>)
        -> Result<Self, DecodeError<S>>
    {
        Self::type_check(&value.type_name, ctx);
        Self::raw_decode(&value.literal, ctx)
    }
}


/// The trait that decodes span into the final structure
pub trait DecodeSpan<S: ErrorSpan>: Sized {
    /// Decode span
    ///
    /// This method can use some extra data (say file name) from the context.
    /// Although, by default context is empty and end users are expected to use
    /// [`parse_with_context`](crate::parse_with_context) to add some values.
    fn decode_span(span: &S, ctx: &mut Context<S>) -> Self;
}

impl<T: ErrorSpan> DecodeSpan<T> for T {
    fn decode_span(span: &T, _: &mut Context<T>) -> Self {
        span.clone()
    }
}

/// Span must implement this trait to be used in the error messages
///
/// Custom span types can be used for this unlike for [`Span`]
pub trait ErrorSpan: Into<miette::SourceSpan>
                     + Clone + fmt::Debug + Send + Sync + 'static {}
impl<T> ErrorSpan for T
    where T: Into<miette::SourceSpan>,
          T: Clone + fmt::Debug + Send + Sync + 'static,
{}


/// Span trait used for parsing source code
///
/// It's sealed because needs some tight interoperation with the parser. Use
/// [`DecodeSpan`] to convert spans whenever needed.
pub trait Span: sealed::Sealed + chumsky::Span + ErrorSpan {}

#[allow(missing_debug_implementations)]
pub(crate) mod sealed {
    pub type Stream<'a, S, T> = chumsky::Stream<
        'a, char, S, Map<std::str::Chars<'a>, T>
    >;

    pub struct Map<I, F>(pub(crate) I, pub(crate) F);

    pub trait SpanTracker {
        type Span;
        fn next_span(&mut self, c: char) -> Self::Span;
    }

    impl<I, T> Iterator for Map<I, T>
         where I: Iterator<Item=char>,
               T: SpanTracker,
    {
        type Item = (char, T::Span);
        fn next(&mut self) -> Option<(char, T::Span)> {
            self.0.next().map(|c| (c, self.1.next_span(c)))
        }
    }

    pub trait Sealed {
        type Tracker: SpanTracker<Span=Self>;
        /// Note assuming ascii, single-width, non-newline chars here
        fn at_start(&self, chars: usize) -> Self;
        fn at_end(&self) -> Self;
        /// Note assuming ascii, single-width, non-newline chars here
        fn before_start(&self, chars: usize) -> Self;
        fn length(&self) -> usize;

        fn stream(s: &str) -> Stream<'_, Self, Self::Tracker>
            where Self: chumsky::Span;
    }
}
