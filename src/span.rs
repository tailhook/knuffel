use std::path::PathBuf;
use std::sync::Arc;

/// Keeps object's boundary positions in the original file
pub(crate) struct Spanned<T, S> {
    span: S,
    value: T,
}

/// Span used for single-file configs
pub struct Span(usize, usize);

/// Span used for configs that are split across different files
pub struct FileSpan(Arc<PathBuf>, Span);


impl<T, S> std::ops::Deref for Spanned<T, S> {
    type Target = T;
    fn deref(&self) -> &T {
        &self.value
    }
}

impl<T, S> Spanned<T, S> {
    pub fn span(&self) -> &S {
        &self.span
    }
}
