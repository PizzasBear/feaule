use super::*;

#[derive(Debug, Clone, Copy)]
pub struct DelimToken {
    pub delim: lexer::Delim,
    pub span: Span,
}

impl_spanned!(DelimToken);

impl Token<'_> for DelimToken {
    #[inline]
    fn expected(span: Span) -> Expected {
        Expected::Group { span }
    }
    #[inline]
    fn matches(token: &TokenTree) -> bool {
        matches!(token.value, TokenTreeValue::Group(_, _))
    }
}

#[derive(Debug, Clone, Copy)]
pub struct ParenToken {
    pub span: Span,
}

impl_spanned!(ParenToken);

impl Token<'_> for ParenToken {
    #[inline]
    fn expected(span: Span) -> Expected {
        Expected::Delim {
            delim: Delim::Paren,
            span,
        }
    }
    #[inline]
    fn matches(token: &TokenTree) -> bool {
        matches!(token.value, TokenTreeValue::Group(Delim::Paren, _))
    }
}

#[derive(Debug, Clone, Copy)]
pub struct BracketToken {
    pub span: Span,
}

impl_spanned!(BracketToken);

impl Token<'_> for BracketToken {
    #[inline]
    fn expected(span: Span) -> Expected {
        Expected::Delim {
            delim: Delim::Bracket,
            span,
        }
    }
    #[inline]
    fn matches(token: &TokenTree) -> bool {
        matches!(token.value, TokenTreeValue::Group(Delim::Bracket, _))
    }
}

#[derive(Debug, Clone, Copy)]
pub struct BraceToken {
    pub span: Span,
}

impl_spanned!(BraceToken);

impl Token<'_> for BraceToken {
    #[inline]
    fn expected(span: Span) -> Expected {
        Expected::Delim {
            delim: Delim::Brace,
            span,
        }
    }
    #[inline]
    fn matches(token: &TokenTree) -> bool {
        matches!(token.value, TokenTreeValue::Group(Delim::Brace, _))
    }
}
