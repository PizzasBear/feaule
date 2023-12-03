use super::*;

#[derive(Debug, Clone)]
pub struct StrToken {
    pub value: Arc<str>,
    pub span: Span,
}

impl_spanned!(StrToken);

impl Token<'_> for StrToken {
    #[inline]
    fn expected(span: Span) -> Expected {
        Expected::Str { span }
    }
    #[inline]
    fn matches(token: &TokenTree) -> bool {
        matches!(token.value, TokenTreeValue::Str(_))
    }
}

impl Parse<'_> for StrToken {
    fn parse(input: &mut ParseBuffer) -> Self {
        input.skip_ignored();
        match input.tokens.first() {
            Some(&TokenTree {
                span,
                value: TokenTreeValue::Str(ref value),
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
pub struct FmtStrToken {
    pub value: Arc<str>,
    pub span: Span,
}

impl_spanned!(FmtStrToken);

impl Token<'_> for FmtStrToken {
    #[inline]
    fn expected(span: Span) -> Expected {
        Expected::FmtStr { span }
    }
    #[inline]
    fn matches(token: &TokenTree) -> bool {
        matches!(token.value, TokenTreeValue::FmtStr(_))
    }
}
