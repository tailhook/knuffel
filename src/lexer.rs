use combine::{StreamOnce, Positioned};

type Error<'a> = combine::easy::Error<Token<'a>, Token<'a>>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum TokenKind {
    String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct Token<'a> {
    pub kind: TokenKind,
    pub value: &'a str,
}

pub(crate) struct TokenStream<'a> {
    data: &'a str,
    position: usize,
}

impl<'a> StreamOnce for TokenStream<'a> {
    type Token = Token<'a>;
    type Range = Token<'a>;
    type Position = usize;
    type Error = combine::easy::Errors<Token<'a>, Token<'a>, usize>;
    fn uncons(&mut self) -> Result<Token<'a>, Error<'a>> {
        todo!();
    }
}

impl<'a> Positioned for TokenStream<'a> {
    fn position(&self) -> usize {
        self.position
    }
}
