use super::*;

#[derive(Debug, Clone, Copy)]
pub struct PureIntToken {
    pub value: u32,
    pub span: Span,
}

impl_spanned!(PureIntToken);

impl Token<'_> for PureIntToken {
    #[inline]
    fn expected(span: Span) -> Expected {
        Expected::PureInt { span }
    }
    #[inline]
    fn matches(token: &TokenTree) -> bool {
        matches!(
            token.value,
            TokenTreeValue::Int(lexer::Int { is_pure: true, .. }),
        )
    }
}

impl Parse<'_> for PureIntToken {
    fn parse(input: &mut ParseBuffer) -> Self {
        input.skip_ignored();
        match input.tokens.first() {
            Some(&TokenTree {
                span,
                value:
                    TokenTreeValue::Int(lexer::Int {
                        value,
                        is_pure: true,
                        ..
                    }),
            }) => {
                input.skip(1);
                Self {
                    value: value as _,
                    span,
                }
            }
            _ => {
                input.push_error(Error::Expected(vec![Self::expected(input.start_span())]));
                return Self {
                    value: Default::default(),
                    span: input.start_span(),
                };
            }
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct IntToken {
    pub value: u128,
    pub ty: lexer::IntType,
    pub span: Span,
}

impl_spanned!(IntToken);

impl Token<'_> for IntToken {
    #[inline]
    fn expected(span: Span) -> Expected {
        Expected::Int { span }
    }
    #[inline]
    fn matches(token: &TokenTree) -> bool {
        matches!(token.value, TokenTreeValue::Int(_))
    }
}

impl Parse<'_> for IntToken {
    fn parse(input: &mut ParseBuffer) -> Self {
        input.skip_ignored();
        match input.tokens.first() {
            Some(&TokenTree {
                span,
                value: TokenTreeValue::Int(lexer::Int { value, ty, .. }),
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
