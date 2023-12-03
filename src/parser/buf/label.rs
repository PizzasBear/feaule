use super::*;

#[derive(Debug, Clone)]
pub struct LabelToken {
    pub name: Arc<str>,
    pub span: Span,
}

impl_spanned!(LabelToken);

impl Token<'_> for LabelToken {
    #[inline]
    fn expected(span: Span) -> Expected {
        Expected::Label { span }
    }
    #[inline]
    fn matches(token: &TokenTree) -> bool {
        matches!(token.value, TokenTreeValue::Label(_))
    }
}

impl Parse<'_> for LabelToken {
    fn parse(input: &mut ParseBuffer) -> Self {
        input.skip_ignored();
        match input.tokens.first() {
            Some(&TokenTree {
                span,
                value: TokenTreeValue::Label(ref value),
            }) => {
                input.skip(1);
                Self {
                    name: value.clone(),
                    span,
                }
            }
            _ => {
                input.push_error(Error::Expected(vec![Self::expected(input.start_span())]));
                return Self {
                    name: "".into(),
                    span: input.start_span(),
                };
            }
        }
    }
}
