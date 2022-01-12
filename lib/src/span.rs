use std::fmt;
use std::ops::Range;

use crate::traits;

/// Reexport of [miette::SourceSpan] trait that we use for parsing
pub use miette::SourceSpan as ErrorSpan;

/// Keeps object's boundary positions in the original file
#[derive(Clone, Debug)]
pub struct Spanned<T, S> {
    pub(crate) span: S,
    pub(crate) value: T,
}

/// Normal byte offset span
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Span(pub usize, pub usize);

// TODO(tailhook) optimize eq to check only offset
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct LinePos {
    line: usize,
    column: usize,
    offset: usize,
}

/// Span with line and column number
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LineSpan(pub LinePos, pub LinePos);


impl Span {
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

impl traits::parsing_span::Sealed for Span {
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

    fn stream(text: &str) -> chumsky::Stream<'_, char, Self, std::iter::Map<std::str::CharIndices<'_>, fn((usize, char)) -> (char, Self)>>
        where Self: chumsky::Span
    {
        chumsky::Stream::from_iter(
            Span(text.len(), text.len()),
            text.char_indices()
                .map(|(i, c)| (c, Span(i, i + c.len_utf8()))),
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

impl traits::parsing_span::Sealed for LineSpan {
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

    fn stream(s: &str) -> chumsky::Stream<'_, char, Self, std::iter::Map<std::str::CharIndices<'_>, fn((usize, char)) -> (char, Self)>>
        where Self: chumsky::Span
    {
        todo!();
    }
}

impl traits::Span for LineSpan {}

impl<T, S> Spanned<T, S> {
    pub fn map<R>(self, f: impl FnOnce(T) -> R) -> Spanned<R, S> {
        Spanned {
            span: self.span,
            value: f(self.value),
        }
    }
}

impl<T, S> std::ops::Deref for Spanned<T, S> {
    type Target = T;
    fn deref(&self) -> &T {
        &self.value
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
