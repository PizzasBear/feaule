use super::*;

#[derive(Debug, Clone, Copy)]
pub struct NewlineToken {
    pub newline: lexer::Newline,
    pub span: Span,
}

impl_spanned!(NewlineToken);

impl Token<'_> for NewlineToken {
    #[inline]
    fn expected(span: Span) -> Expected {
        Expected::Newline { span }
    }
    #[inline]
    fn matches(token: &TokenTree) -> bool {
        matches!(token.value, TokenTreeValue::Newline(_))
    }

    fn peek(input: &ParseBuffer) -> bool {
        let mut input = input.fork();
        input.ignore_newline = false;
        input.skip_ignored();
        Self::matches(&input.tokens[0])
    }
    fn peek2(input: &ParseBuffer) -> bool {
        let mut input = input.fork();
        input.ignore_newline = false;
        input.skip_ignored();
        input.skip(1);
        input.skip_ignored();
        Self::matches(&input.tokens[0])
    }
    fn peek3(input: &ParseBuffer) -> bool {
        let mut input = input.fork();
        input.ignore_newline = false;
        input.skip_ignored();
        input.skip(1);
        input.skip_ignored();
        input.skip(1);
        input.skip_ignored();
        Self::matches(&input.tokens[0])
    }
}

impl Parse<'_> for NewlineToken {
    fn parse(input: &mut ParseBuffer) -> Self {
        let old_ignore_newline = mem::replace(&mut input.ignore_newline, false);
        input.skip_ignored();
        input.ignore_newline = old_ignore_newline;

        match input.tokens.first() {
            Some(&TokenTree {
                span,
                value: TokenTreeValue::Newline(newline),
            }) => {
                input.skip(1);
                Self { newline, span }
            }
            _ => {
                input.push_error(Error::Expected(vec![Self::expected(input.start_span())]));
                return Self {
                    newline: lexer::Newline,
                    span: input.start_span(),
                };
            }
        }
    }
}
