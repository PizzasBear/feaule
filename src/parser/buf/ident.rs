use super::*;

#[derive(Debug, Clone)]
pub struct AnyIdentToken {
    pub raw: bool,
    pub name: Arc<str>,
    pub span: Span,
}

impl_spanned!(AnyIdentToken);

impl Token<'_> for AnyIdentToken {
    #[inline]
    fn expected(span: Span) -> Expected {
        Expected::AnyIdent { span }
    }
    #[inline]
    fn matches(token: &TokenTree) -> bool {
        matches!(token.value, TokenTreeValue::Ident { .. })
    }
}

impl Parse<'_> for AnyIdentToken {
    fn parse(input: &mut ParseBuffer) -> Self {
        input.skip_ignored();
        match input.tokens.first() {
            Some(&TokenTree {
                span,
                value: TokenTreeValue::Ident { raw, ref ident },
            }) => {
                input.skip(1);
                Self {
                    raw,
                    name: ident.clone(),
                    span,
                }
            }
            _ => {
                input.push_error(Error::Expected(vec![Self::expected(input.start_span())]));
                return Self {
                    raw: false,
                    name: "".into(),
                    span: input.start_span(),
                };
            }
        }
    }
}

macro_rules! def_kw_tokens {
    ($($(#[$attr:meta])* $name:ident[$kw:ident]),+ $(,)?) => {
        $(
            #[derive(Debug, Clone, Copy)]
            $(#[$attr])*
            pub struct $name {
                pub span: Span,
            }

            impl $name {
                pub const STR: &'static str = stringify!($kw);
            }

            impl_spanned!($name);

            impl Token<'_> for $name {
                #[inline]
                fn expected(span: Span) -> Expected {
                    Expected::Keyword { span, kw: Self::STR }
                }
                #[inline]
                fn matches(token: &TokenTree) -> bool {
                    match &token.value {
                        TokenTreeValue::Ident { raw: false, ident } => &**ident == Self::STR,
                        _ => false
                    }
                }
            }

            impl Parse<'_> for $name {
                fn parse(input: &mut ParseBuffer) -> Self {
                    input.skip_ignored();
                    match input.tokens.first() {
                        Some(&TokenTree {
                            span,
                            value: TokenTreeValue::Ident { raw: false, ref ident },
                        }) if &**ident == Self::STR => {
                            // Maybe check raw identifiers here.
                            input.skip(1);
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

        pub const KEYWORDS: &[&str] = &[$(stringify!($kw)),+];

        #[derive(Debug, Clone)]
        pub struct IdentToken {
            pub raw: bool,
            pub name: Arc<str>,
            pub span: Span,
        }

        impl_spanned!(IdentToken);

        impl Token<'_> for IdentToken {
            #[inline]
            fn expected(span: Span) -> Expected {
                Expected::Ident { span }
            }
            #[inline]
            fn matches(token: &TokenTree) -> bool {
                match &token.value {
                    TokenTreeValue::Ident { raw, ident } => *raw || !KEYWORDS.contains(&&**ident),
                    _ => false
                }
            }
        }

        impl Parse<'_> for IdentToken {
            fn parse(input: &mut ParseBuffer) -> Self {
                input.skip_ignored();
                match input.tokens.first() {
                    Some(tt @ &TokenTree {
                        span,
                        value: TokenTreeValue::Ident { raw, ref ident },
                    }) if Self::matches(tt) => {
                        input.skip(1);
                        Self {
                            raw,
                            name: ident.clone(),
                            span,
                        }
                    }
                    _ => {
                        input.push_error(Error::Expected(vec![Self::expected(input.start_span())]));
                        return Self {
                            raw: false,
                            name: "".into(),
                            span: input.start_span(),
                        };
                    }
                }
            }
        }

        #[macro_export]
        macro_rules! keyword_token {
            $(($kw) => { $crate::parser::buf::$name };)+
        }
        pub use keyword_token;
    };
}

def_kw_tokens! {
    /// `const` keyword
    ConstToken[const],
    /// `mut` keyword
    MutToken[mut],
    /// `if` keyword
    IfToken[if],
    /// `else` keyword
    ElseToken[else],
    /// `await` keyword
    AwaitToken[await],
    /// `fn` keyword
    FnToken[fn],
    /// `struct` keyword
    StructToken[struct],
    /// `class` keyword
    ClassToken[class],
    /// `trait` keyword
    TraitToken[trait],
    /// `interface` keyword
    InterfaceToken[interface],
    /// `return` keyword
    ReturnToken[return],
    /// `for` keyword
    ForToken[for],
    /// `while` keyword
    WhileToken[while],
    /// `yield` keyword
    YieldToken[yield],
    /// `let` keyword
    LetToken[let],
    /// `var` keyword
    VarToken[var],
    /// `do` keyword
    DoToken[do],
    /// `in` keyword
    InToken[in],
    /// `of` keyword
    OfToken[of],
    /// `immut` keyword
    ImmutToken[immut],
    /// `where` keyword
    WhereToken[where],
    /// `async` keyword
    AsyncToken[async],
    /// `throw` keyword
    ThrowToken[throw],
    /// `raise` keyword
    RaiseToken[raise],
    /// `try` keyword
    TryToken[try],
    /// `except` keyword
    ExceptToken[except],
    /// `catch` keyword
    CatchToken[catch],
    /// `finally` keyword
    FinallyToken[finally],
    /// `operator` keyword
    OperatorToken[operator],
    /// `match` keyword
    MatchToken[match],
    /// `switch` keyword
    SwitchToken[switch],
    /// `case` keyword
    CaseToken[case],
}
