use super::*;

#[derive(Debug, Clone, Copy)]
pub struct PunctToken {
    pub punct: Punct,
    pub span: Span,
}

impl_spanned!(PunctToken);

impl Token<'_> for PunctToken {
    #[inline]
    fn expected(span: Span) -> Expected {
        Expected::AnyPunct { span }
    }
    #[inline]
    fn matches(token: &TokenTree) -> bool {
        matches!(token.value, TokenTreeValue::Punct(_))
    }
}

impl Parse<'_> for PunctToken {
    fn parse(input: &mut ParseBuffer) -> Self {
        input.skip_ignored();
        match input.tokens.first() {
            Some(&TokenTree {
                span,
                value: TokenTreeValue::Punct(punct),
            }) => {
                input.skip(1);
                Self { punct, span }
            }
            _ => {
                input.push_error(Error::Expected(vec![Self::expected(input.start_span())]));
                return Self {
                    punct: punct![End],
                    span: input.start_span(),
                };
            }
        }
    }
}

macro_rules! def_punct_seq_tokens {
    (@ignore ($($i:tt)*) $(return $($ret:tt)*)?) => {
        $($($ret)*)?
    };
    (@count_tokens $($tt:tt)*) => {
        (0 $(+ def_punct_seq_tokens!(@ignore ($tt) return 1))+)
    };
    (@get_lit [$($match:tt)+]) => { stringify!($($match)+) };
    (@get_lit $_:tt = $lit:literal) => { $lit };
    ($($(#[$attr:meta])* [$($match:tt)+] => $name:ident[$($punct:tt)+] $(= $lit:literal)?),+ $(,)?) => {
        $(
            $(#[$attr])*
            #[derive(Clone, Copy)]
            pub struct $name {
                pub span: Span,
            }

            impl fmt::Debug for $name {
                fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                    write!(f, "punct_token![{:?} @ {:?}]", Self::STR, self.span)
                }
            }

            impl $name {
                pub const STR: &'static str = def_punct_seq_tokens!(@get_lit [$($match)+] $(= $lit)?);
            }

            impl_spanned!($name);

            impl Token<'_> for $name {
                #[inline]
                fn expected(span: Span) -> Expected {
                    Expected::Punct { span, punct: Self::STR }
                }
                fn matches(token: &TokenTree) -> bool {
                    const PUNCTS: &[Punct] = &punct![$([$punct],)+];
                    const FIRST_PUNCT: Punct = PUNCTS[0];
                    if PUNCTS.len() == 1 {
                        matches!(token.value, TokenTreeValue::Punct(FIRST_PUNCT))
                    } else {
                        false
                    }
                }

                fn peek(input: &ParseBuffer) -> bool {
                    let mut input = input.fork();
                    input.skip_ignored();
                    match input.tokens {
                        [
                            $(TokenTree {
                                value: TokenTreeValue::Punct(punct![$punct]),
                                ..
                            },)+
                            ..
                        ] => true,
                        _ => false,
                    }
                }
                fn peek2(input: &ParseBuffer) -> bool {
                    let mut input = input.fork();
                    input.skip_ignored();
                    input.skip(1);
                    input.skip_ignored();
                    match input.tokens {
                        [
                            $(TokenTree {
                                value: TokenTreeValue::Punct(punct![$punct]),
                                ..
                            },)+
                            ..
                        ] => true,
                        _ => false,
                    }
                }
                fn peek3(input: &ParseBuffer) -> bool {
                    let mut input = input.fork();
                    input.skip_ignored();
                    input.skip(1);
                    input.skip_ignored();
                    input.skip(1);
                    input.skip_ignored();
                    match input.tokens {
                        [
                            $(TokenTree {
                                value: TokenTreeValue::Punct(punct![$punct]),
                                ..
                            },)+
                            ..
                        ] => true,
                        _ => false,
                    }
                }
            }

            impl Parse<'_> for $name {
                fn parse(input: &mut ParseBuffer) -> Self {
                    const PUNCT_COUNT: usize = def_punct_seq_tokens!(
                        @count_tokens $($punct)+
                    );

                    input.skip_ignored();
                    match input.tokens {
                        &[
                            $(TokenTree {
                                value: TokenTreeValue::Punct(punct![$punct]),
                                ..
                            },)+
                            ..
                        ] => {
                            let span = input.span
                                .merged_with(&input.tokens[PUNCT_COUNT - 1].span)
                                .unwrap();
                            input.skip(PUNCT_COUNT);
                            Self {
                                span,
                            }
                        }
                        _ => {
                            input.push_error(Error::Expected(vec![Self::expected(
                                input.start_span(),
                            )]));
                            return Self {
                                span: input.start_span(),
                            };
                        }
                    }
                }
            }
        )+
        #[macro_export]
        macro_rules! punct_token {
            $([$($match)+] => { $crate::parser::buf::$name };)+
        }
        pub use punct_token;
    };
}

def_punct_seq_tokens! {
    /// Plus Sign `+`
    [+] => PlusToken[+],
    /// Minus Sign `-`
    [-] => MinusToken[-],
    /// Asterisk `*`
    [*] => AstToken[*],
    /// Solidus `/`
    [/] => SolToken[/],
    /// Percent Sign `%`
    [%] => PercntToken[%],
    /// Exclamation Mark `!`
    [!] => ExclToken[!],
    /// Ampersand `&`
    [&] => AmpToken[&],
    /// Vertical Line `|`
    [|] => VerbarToken[|],
    /// Circumflex Accent `^`
    [^] => HatToken[^],
    /// Tilde Symbol `~`
    [~] => TildeToken[~],
    /// Dollar Sign `$`
    [$] => DollarToken[$],
    /// Commercial At Symbol `@`
    [@] => CommatToken[@],
    /// Less Than Sign `<`
    [<] => LtToken[<],
    /// Greater Than Sign `>`
    [>] => GtToken[>],
    /// Equals Sign `=`
    [=] => EqualsToken[=],
    /// Full Stop `.`
    [.] => PeriodToken[.],
    /// Comma `,`
    [,] => CommaToken[,],
    /// Colon `:`
    [:] => ColonToken[:],
    /// Semicolon `;`
    [;] => SemiToken[;],
    /// Question Mark `?`
    [?] => QuestToken[?],
    /// Grave Accent '`'
    [Grave] => GraveToken[Grave] = "`",
    /// Reverse Solidus `\`
    [Bsol] => BsolToken[Bsol] = r"\",

    // ==== multi-punct ====

    /// Range `..`
    [..] => RangeToken[. .],
    /// Inclusive Range `..=`
    [..=] => InclusiveRangeToken[. .=],
}
