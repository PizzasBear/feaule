use super::*;

#[derive(Debug, Clone, Copy)]
pub struct CharToken {
    pub value: char,
    pub span: Span,
}

impl_spanned!(CharToken);

impl Token<'_> for CharToken {
    #[inline]
    fn expected(span: Span) -> Expected {
        Expected::Char { span }
    }
    #[inline]
    fn matches(token: &TokenTree) -> bool {
        matches!(token.value, TokenTreeValue::Char(_))
    }
}

impl Parse<'_> for CharToken {
    fn parse(input: &mut ParseBuffer) -> Self {
        input.skip_ignored();
        match input.tokens.first() {
            Some(&TokenTree {
                span,
                value: TokenTreeValue::Char(value),
            }) => {
                input.skip(1);
                Self { value, span }
            }
            _ => {
                input.push_error(Error::Expected(vec![Self::expected(input.start_span())]));
                return Self {
                    value: '\0',
                    span: input.start_span(),
                };
            }
        }
    }
}
