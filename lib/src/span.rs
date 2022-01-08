use std::fmt;
use std::path::PathBuf;
use std::sync::Arc;

use crate::traits;

/// Keeps object's boundary positions in the original file
#[derive(Clone, Debug)]
pub struct Spanned<T, S> {
    pub(crate) span: S,
    pub(crate) value: T,
}

/// Span used for single-file configs
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Span(pub usize, pub usize);

/// Span used for configs that are split across different files
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FileSpan(pub Arc<PathBuf>, pub Span);

impl traits::Span for Span {}
impl traits::Span for FileSpan {}

impl Span {
    pub fn length(&self) -> usize {
        self.1.saturating_sub(self.0)
    }
}

impl Into<miette::SourceSpan> for Span {
    fn into(self: Span) -> miette::SourceSpan {
        (self.0, self.1 - self.0).into()
    }
}

impl Into<miette::SourceSpan> for FileSpan {
    fn into(self: FileSpan) -> miette::SourceSpan {
        self.1.into()
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

impl chumsky::Span for FileSpan {
    type Context = Arc<PathBuf>;
    type Offset = usize;
    fn new(context: Arc<PathBuf>, range: std::ops::Range<usize>) -> Self {
        FileSpan(context, Span(range.start(), range.end()))
    }
    fn context(&self) -> Arc<PathBuf> { self.0.clone() }
    fn start(&self) -> usize { (self.1).0 }
    fn end(&self) -> usize { (self.1).1 }
}

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

impl fmt::Display for FileSpan {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.display().fmt(f)?;
        ":".fmt(f)?;
        self.1.fmt(f)?;
        Ok(())
    }
}
