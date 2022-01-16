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
//! more elaborate thing that includes file name or can refer to a default
//! configuration value. See [`traits::DecodeSpan`].
use std::fmt;
use std::ops::Range;

use crate::traits;

/// Reexport of [miette::SourceSpan] trait that we use for parsing
pub use miette::SourceSpan as ErrorSpan;

/// Wraps the structure to keep source code span, but also dereference to T
#[derive(Clone, Debug)]
pub struct Spanned<T, S> {
    pub(crate) span: S,
    pub(crate) value: T,
}

/// Normal byte offset span
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Span(pub usize, pub usize);

/// Line and column position of the datum in the source code
// TODO(tailhook) optimize Eq to check only offset
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct LinePos {
    /// Zero-based line number
    pub line: usize,
    /// Zero-based column number
    pub column: usize,
    /// Zero-based byte offset
    pub offset: usize,
}

/// Span with line and column number
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LineSpan(pub LinePos, pub LinePos);

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

impl traits::Span for LineSpan {}

impl<T, S> Spanned<T, S> {
    /// Converts value but keeps the same span attached
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
