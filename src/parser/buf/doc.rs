use super::*;

#[derive(Debug, Clone)]
pub struct InnerDocToken {
    pub value: Arc<str>,
    pub span: Span,
}

impl_spanned!(InnerDocToken);

impl Token<'_> for InnerDocToken {
    #[inline]
    fn expected(span: Span) -> Expected {
        Expected::InnerDoc { span }
    }
    #[inline]
    fn matches(token: &TokenTree) -> bool {
        matches!(token.value, TokenTreeValue::InnerDoc(_))
    }
}

impl Parse<'_> for InnerDocToken {
    fn parse(input: &mut ParseBuffer) -> Self {
        input.skip_ignored();
        match input.tokens.first() {
            Some(&TokenTree {
                span,
                value: TokenTreeValue::InnerDoc(ref value),
            }) => {
                input.skip(1);
                Self {
                    value: value.clone(),
                    span,
                }
            }
            _ => {
                input.push_error(Error::Expected(vec![Self::expected(input.start_span())]));
                return Self {
                    value: "".into(),
                    span: input.start_span(),
                };
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct OuterDocToken {
    pub value: Arc<str>,
    pub span: Span,
}

impl_spanned!(OuterDocToken);

impl Token<'_> for OuterDocToken {
    #[inline]
    fn expected(span: Span) -> Expected {
        Expected::OuterDoc { span }
    }
    #[inline]
    fn matches(token: &TokenTree) -> bool {
        matches!(token.value, TokenTreeValue::OuterDoc(_))
    }
}

impl Parse<'_> for OuterDocToken {
    fn parse(input: &mut ParseBuffer) -> Self {
        input.skip_ignored();
        match input.tokens.first() {
            Some(&TokenTree {
                span,
                value: TokenTreeValue::OuterDoc(ref value),
            }) => {
                input.skip(1);
                Self {
                    value: value.clone(),
                    span,
                }
            }
            _ => {
                input.push_error(Error::Expected(vec![Self::expected(input.start_span())]));
                return Self {
                    value: "".into(),
                    span: input.start_span(),
                };
            }
        }
    }
}
