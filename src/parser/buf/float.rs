use super::*;

#[derive(Debug, Clone, Copy)]
pub struct FloatToken {
    pub value: f64,
    pub ty: lexer::FloatType,
    pub span: Span,
}

impl_spanned!(FloatToken);

impl Token<'_> for FloatToken {
    #[inline]
    fn expected(span: Span) -> Expected {
        Expected::Float { span }
    }
    #[inline]
    fn matches(token: &TokenTree) -> bool {
        matches!(token.value, TokenTreeValue::Float(_))
    }
}

impl Parse<'_> for FloatToken {
    fn parse(input: &mut ParseBuffer) -> Self {
        input.skip_ignored();
        match input.tokens.first() {
            Some(&TokenTree {
                span,
                value: TokenTreeValue::Float(lexer::Float { value, ty, .. }),
            }) => {
                input.skip(1);
                Self { value, ty, span }
            }
            _ => {
                input.push_error(Error::Expected(vec![Self::expected(input.start_span())]));
                return Self {
                    value: Default::default(),
                    ty: Default::default(),
                    span: input.start_span(),
                };
            }
        }
    }
}
