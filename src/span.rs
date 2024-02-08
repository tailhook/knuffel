//! Knuffel supports to kinds of the span for parsing
//!
//! 1. [`Span`] which only tracks byte offset from the start of the source code
//! 2. [`LineSpan`] which also track line numbers
//!
//! This distinction is important during parsing stage as [`Span`] is normally
//! faster. And [`LineSpan`] is still faster than find out line/column number
//! for each span separately, and is also more convenient if you need this
//! information.
//!
//! On the other hand, on the decode stage you can convert your span types into
//! more elaborate thing that includes file name or can refer to the defaults
//! as a separate kind of span. See [`traits::DecodeSpan`].
use std::fmt;
use std::ops::Range;

use crate::traits;
use crate::decode::Context;

/// Reexport of [miette::SourceSpan] trait that we use for parsing
pub use miette::SourceSpan as ErrorSpan;

/// Wraps the structure to keep source code span, but also dereference to T
#[derive(Clone, Copy, Debug)]
#[cfg_attr(feature="minicbor", derive(minicbor::Encode, minicbor::Decode))]
pub struct Spanned<T, S> {
    #[cfg_attr(feature="minicbor", n(0))]
    pub(crate) span: S,
    #[cfg_attr(feature="minicbor", n(1))]
    pub(crate) value: T,
}

/// Normal byte offset span
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[cfg_attr(feature="minicbor", derive(minicbor::Encode, minicbor::Decode))]
pub struct Span(
    #[cfg_attr(feature="minicbor", n(0))]
    pub usize,
    #[cfg_attr(feature="minicbor", n(1))]
    pub usize,
);

/// Line and column position of the datum in the source code
// TODO(tailhook) optimize Eq to check only offset
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[cfg_attr(feature="minicbor", derive(minicbor::Encode, minicbor::Decode))]
pub struct LinePos {
    /// Zero-based byte offset
    #[cfg_attr(feature="minicbor", n(0))]
    pub offset: usize,
    /// Zero-based line number
    #[cfg_attr(feature="minicbor", n(1))]
    pub line: usize,
    /// Zero-based column number
    #[cfg_attr(feature="minicbor", n(2))]
    pub column: usize,
}

/// Span with line and column number
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[cfg_attr(feature="minicbor", derive(minicbor::Encode, minicbor::Decode))]
pub struct LineSpan(
    #[cfg_attr(feature="minicbor", n(0))]
    pub LinePos,
    #[cfg_attr(feature="minicbor", n(1))]
    pub LinePos,
);

#[allow(missing_debug_implementations)]
mod sealed {

    pub struct OffsetTracker {
        pub(crate) offset: usize,
    }

    #[cfg(feature="line-numbers")]
    pub struct LineTracker {
        pub(crate) offset: usize,
        pub(crate) caret_return: bool,
        pub(crate) line: usize,
        pub(crate) column: usize,
    }

}


impl Span {
    /// Length of the span in bytes
    pub fn length(&self) -> usize {
        self.1.saturating_sub(self.0)
    }
}

impl Into<ErrorSpan> for Span {
    fn into(self) -> ErrorSpan {
        (self.0, self.1.saturating_sub(self.0)).into()
    }
}

impl Into<ErrorSpan> for LineSpan {
    fn into(self) -> ErrorSpan {
        (self.0.offset, self.1.offset.saturating_sub(self.0.offset)).into()
    }
}

impl chumsky::Span for Span {
    type Context = ();
    type Offset = usize;
    fn new(_context: (), range: std::ops::Range<usize>) -> Self {
        Span(range.start(), range.end())
    }
    fn context(&self) -> () { () }
    fn start(&self) -> usize { self.0 }
    fn end(&self) -> usize { self.1 }
}
impl traits::sealed::SpanTracker for sealed::OffsetTracker {
    type Span = Span;
    fn next_span(&mut self, c: char) -> Span {
        let start = self.offset;
        self.offset += c.len_utf8();
        Span(start, self.offset)
    }
}


impl traits::sealed::Sealed for Span {
    type Tracker = sealed::OffsetTracker;
    fn at_start(&self, chars: usize) -> Self {
        Span(self.0, self.0+chars)
    }

    fn at_end(&self) -> Self {
        Span(self.1, self.1)
    }

    fn before_start(&self, chars: usize) -> Self {
        Span(self.0.saturating_sub(chars), self.0)
    }

    fn length(&self) -> usize {
        self.1.saturating_sub(self.0)
    }

    fn stream(text: &str) -> traits::sealed::Stream<'_, Self, Self::Tracker>
        where Self: chumsky::Span
    {
        chumsky::Stream::from_iter(
            Span(text.len(), text.len()),
            traits::sealed::Map(text.chars(),
                                sealed::OffsetTracker { offset: 0 }),
        )
    }
}

impl traits::Span for Span {}

impl chumsky::Span for LineSpan {
    type Context = ();
    type Offset = LinePos;
    fn new(_context: (), range: std::ops::Range<LinePos>) -> Self {
        LineSpan(range.start, range.end)
    }
    fn context(&self) -> () { () }
    fn start(&self) -> LinePos { self.0 }
    fn end(&self) -> LinePos { self.1 }
}

#[cfg(feature="line-numbers")]
impl traits::sealed::SpanTracker for sealed::LineTracker {
    type Span = LineSpan;
    fn next_span(&mut self, c: char) -> LineSpan {
        let offset = self.offset;
        let line = self.line;
        let column = self.column;
        self.offset += c.len_utf8();
        match c {
            '\n' if self.caret_return => {}
            '\r'|'\n'|'\x0C'|'\u{0085}'|'\u{2028}'|'\u{2029}' => {
                self.line += 1;
                self.column = 0;
            }
            '\t' => self.column += 8,
            c => {
                self.column += unicode_width::UnicodeWidthChar::width(c)
                    .unwrap_or(0);  // treat control chars as zero-length
            }
        }
        self.caret_return = c == '\r';
        LineSpan(
            LinePos {
                line,
                column,
                offset,
            },
            LinePos {
                line: self.line,
                column: self.column,
                offset: self.offset,
            },
        )
    }
}

#[cfg(feature="line-numbers")]
impl traits::sealed::Sealed for LineSpan {
    type Tracker = sealed::LineTracker;
    /// Note assuming ascii, single-width, non-newline chars here
    fn at_start(&self, chars: usize) -> Self {
        LineSpan(self.0, LinePos {
            offset: self.0.offset + chars,
            column: self.0.column + chars,
            .. self.0
        })
    }

    fn at_end(&self) -> Self {
        LineSpan(self.1, self.1)
    }

    /// Note assuming ascii, single-width, non-newline chars here
    fn before_start(&self, chars: usize) -> Self {
        LineSpan(LinePos {
            offset: self.0.offset.saturating_sub(chars),
            column: self.0.column.saturating_sub(chars),
            .. self.0
        }, self.0)
    }

    fn length(&self) -> usize {
        self.1.offset.saturating_sub(self.0.offset)
    }

    fn stream(text: &str) -> traits::sealed::Stream<'_, Self, Self::Tracker>
        where Self: chumsky::Span
    {
        let mut caret_return = false;
        let mut line = 0;
        let mut last_line = text;
        let mut iter = text.chars();
        while let Some(c) = iter.next() {
            match c {
                '\n' if caret_return => {}
                '\r'|'\n'|'\x0C'|'\u{0085}'|'\u{2028}'|'\u{2029}' => {
                    line += 1;
                    last_line = iter.as_str();
                }
                _ => {}
            }
            caret_return = c == '\r';
        }
        let column = unicode_width::UnicodeWidthStr::width(last_line);
        let eoi = LinePos {
            line,
            column,
            offset: text.len(),
        };
        chumsky::Stream::from_iter(
            LineSpan(eoi, eoi),
            traits::sealed::Map(
                text.chars(),
                sealed::LineTracker {
                    caret_return: false,
                    offset: 0,
                    line: 0,
                    column: 0,
                },
            ),
        )
    }
}

#[cfg(feature="line-numbers")]
impl traits::Span for LineSpan {}

#[cfg(feature="line-numbers")]
impl traits::DecodeSpan<LineSpan> for Span {
    fn decode_span(span: &LineSpan, _: &mut Context<LineSpan>)
        -> Self
    {
        Span(span.0.offset, span.1.offset)
    }
}

impl<T, S> Spanned<T, S> {
    /// Converts value but keeps the same span attached
    pub fn map<R>(self, f: impl FnOnce(T) -> R) -> Spanned<R, S> {
        Spanned {
            span: self.span,
            value: f(self.value),
        }
    }
    /// Converts span but keeps the same value attached
    pub fn map_span<U>(self, f: impl FnOnce(S) -> U) -> Spanned<T, U> {
        Spanned {
            span: f(self.span),
            value: self.value,
        }
    }
    pub(crate) fn clone_as<U>(&self, ctx: &mut Context<S>) -> Spanned<T, U>
        where U: traits::DecodeSpan<S>,
              T: Clone,
              S: traits::ErrorSpan,
    {
        Spanned {
            span: traits::DecodeSpan::decode_span(&self.span, ctx),
            value: self.value.clone(),
        }
    }
}

impl<U: ?Sized, T: AsRef<U>, S> AsRef<U> for Spanned<T, S> {
    fn as_ref(&self) -> &U {
        self.value.as_ref()
    }
}

impl<U: ?Sized, T: AsMut<U>, S> AsMut<U> for Spanned<T, S> {
    fn as_mut(&mut self) -> &mut U {
        self.value.as_mut()
    }
}

impl<T, S> std::ops::Deref for Spanned<T, S> {
    type Target = T;
    fn deref(&self) -> &T {
        &self.value
    }
}

impl<T, S> std::ops::DerefMut for Spanned<T, S> {
    fn deref_mut(&mut self) -> &mut T {
        &mut self.value
    }
}

impl<T, S> std::borrow::Borrow<T> for Spanned<T, S> {
    fn borrow(&self) -> &T {
        self.value.borrow()
    }
}

impl<T: ?Sized, S> std::borrow::Borrow<T> for Spanned<Box<T>, S> {
    fn borrow(&self) -> &T {
        self.value.borrow()
    }
}

impl<T, S> Spanned<T, S> {
    /// Returns the span of the value
    pub fn span(&self) -> &S {
        &self.span
    }
}

impl<S, T: PartialEq<T>> PartialEq for Spanned<T, S> {
    fn eq(&self, other: &Spanned<T, S>) -> bool {
        self.value == other.value
    }
}

impl<S, T: PartialOrd<T>> PartialOrd for Spanned<T, S> {
    fn partial_cmp(&self, other: &Spanned<T, S>)
        -> Option<std::cmp::Ordering>
    {
        self.value.partial_cmp(&other.value)
    }
}

impl<S, T: Ord> Ord for Spanned<T, S> {
    fn cmp(&self, other: &Spanned<T, S>) -> std::cmp::Ordering {
        self.value.cmp(&other.value)
    }
}

impl<S, T: Eq> Eq for Spanned<T, S> {}

impl<S, T: std::hash::Hash> std::hash::Hash for Spanned<T, S> {
    fn hash<H>(&self, state: &mut H)
        where H: std::hash::Hasher,
    {
        self.value.hash(state)
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)?;
        "..".fmt(f)?;
        self.1.fmt(f)?;
        Ok(())
    }
}

impl From<Range<usize>> for Span {
    fn from(r: Range<usize>) -> Span {
        Span(r.start, r.end)
    }
}
