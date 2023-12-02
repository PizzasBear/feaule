use std::{
    fmt,
    iter::FusedIterator,
    mem,
    sync::{Arc, Mutex},
};

use crate::{
    code::Span,
    lexer::{self, punct, Delim, Punct, TokenTree, TokenTreeValue},
};

use super::{Error, Expected};

// TODO:
// - implement Punctuated
// - implement parse_terminated

#[derive(Debug)]
pub struct ParseBuffer<'a> {
    span: Span,
    tokens: &'a [TokenTree],
    pub ignore_newline: bool,
    errors: &'a Mutex<Vec<Error>>,
}

impl<'a> ParseBuffer<'a> {
    /// Skips `n` tokens
    fn skip(&mut self, n: usize) {
        let new_start_pos = self
            .tokens
            .get(n)
            .map_or(self.span.end_pos(), |tk| tk.span.start_pos());
        self.span = new_start_pos.to(self.span.end_pos()).unwrap();
        self.tokens = &self.tokens[n..];
    }

    fn skip_ignored(&mut self) {
        let n = self
            .tokens
            .iter()
            .position(|tk| match tk.value {
                TokenTreeValue::Newline(_) => !self.ignore_newline,
                TokenTreeValue::Punct(Punct::End) => false,
                _ => true,
            })
            .unwrap_or(self.tokens.len());
        self.skip(n);
    }

    #[inline]
    pub const fn total_span(&self) -> Span {
        self.span
    }

    #[inline]
    pub const fn start_span(&self) -> Span {
        self.span.start_span()
    }

    #[inline]
    pub const fn is_empty(&self) -> bool {
        self.tokens.is_empty()
    }

    #[inline]
    pub fn push_error(&self, error: Error) {
        self.errors.lock().unwrap().push(error);
    }

    #[inline]
    pub fn fork(&self) -> Self {
        Self { ..*self }
    }

    #[inline]
    pub fn parse<T: Parse<'a>>(&mut self) -> T {
        T::parse(self)
    }

    #[inline]
    pub fn lookahead1(&self) -> Lookahead1 {
        Lookahead1 {
            input: self,
            expected: vec![],
        }
    }

    #[inline]
    pub fn peek<T: Token<'a>>(&self) -> bool {
        T::peek(self)
    }

    #[inline]
    pub fn peek2<T: Token<'a>>(&self) -> bool {
        T::peek2(self)
    }

    #[inline]
    pub fn peek3<T: Token<'a>>(&self) -> bool {
        T::peek3(self)
    }

    pub fn parse_fmt_str(
        &mut self,
    ) -> (
        FmtStrToken,
        impl Iterator<Item = (usize, Self)> + DoubleEndedIterator + ExactSizeIterator + FusedIterator,
    ) {
        self.skip_ignored();
        let (span, value, args) = match self.tokens.first() {
            Some(&TokenTree {
                span,
                value:
                    TokenTreeValue::FmtStr(lexer::FmtStr {
                        ref value,
                        ref args,
                    }),
            }) => {
                self.skip(1);
                (span, value.clone(), args.as_slice())
            }
            _ => {
                self.push_error(Error::Expected(vec![FmtStrToken::expected(
                    self.start_span(),
                )]));
                (self.start_span(), "".into(), [].as_slice())
            }
        };

        let errors = self.errors;
        let ignore_newline = self.ignore_newline;
        (
            FmtStrToken { span, value },
            args.iter().map(move |arg| {
                (
                    arg.index,
                    Self {
                        span: match arg.tokens.as_slice() {
                            [] => arg.span,
                            [f @ l] | [f, .., l] => f.span.merged_with(&l.span).unwrap(),
                        },
                        tokens: &arg.tokens,
                        ignore_newline,
                        errors,
                    },
                )
            }),
        )
    }

    pub fn parse_paren(&mut self) -> (ParenToken, Self) {
        let (tk, inner) = self.parse_group(Some(lexer::Delim::Paren));
        debug_assert_eq!(tk.delim, lexer::Delim::Paren);
        (ParenToken { span: tk.span }, inner)
    }

    pub fn parse_bracket(&mut self) -> (BracketToken, Self) {
        let (tk, inner) = self.parse_group(Some(lexer::Delim::Bracket));
        debug_assert_eq!(tk.delim, lexer::Delim::Bracket);
        (BracketToken { span: tk.span }, inner)
    }

    pub fn parse_brace(&mut self) -> (BraceToken, Self) {
        let (tk, inner) = self.parse_group(Some(lexer::Delim::Brace));
        debug_assert_eq!(tk.delim, lexer::Delim::Brace);
        (BraceToken { span: tk.span }, inner)
    }

    pub fn parse_group(&mut self, delim: Option<lexer::Delim>) -> (DelimToken, Self) {
        self.skip_ignored();
        match self.tokens.first() {
            Some(&TokenTree {
                span,
                value: TokenTreeValue::Group(tk_delim, ref tokens),
            }) => {
                self.skip(1);
                match delim {
                    Some(delim) if delim != tk_delim => {
                        self.push_error(Error::Expected(vec![Expected::Delim { delim, span }]));
                    }
                    _ => {}
                }
                (
                    DelimToken {
                        span,
                        delim: delim.unwrap_or(tk_delim),
                    },
                    Self {
                        span: match tokens.as_slice() {
                            [] => span,
                            [f @ l] | [f, .., l] => f.span.merged_with(&l.span).unwrap(),
                        },
                        tokens,
                        ..*self
                    },
                )
            }
            _ => {
                self.push_error(Error::Expected(vec![match delim {
                    Some(delim) => Expected::Delim {
                        delim,
                        span: self.start_span(),
                    },
                    None => Expected::Group {
                        span: self.start_span(),
                    },
                }]));
                (
                    DelimToken {
                        delim: delim.unwrap_or(lexer::Delim::None),
                        span: self.start_span(),
                    },
                    Self {
                        span: self.start_span(),
                        tokens: &[],
                        ..*self
                    },
                )
            }
        }
    }
}

impl Drop for ParseBuffer<'_> {
    fn drop(&mut self) {
        if let Some(tk) = self.tokens.first() {
            self.push_error(Error::UnexpectedToken { span: tk.span });
        }
    }
}

pub trait Parse<'a> {
    fn parse(input: &mut ParseBuffer<'a>) -> Self;
}

pub trait Token<'a> {
    fn expected(span: Span) -> Expected;
    fn matches(token: &TokenTree) -> bool;

    fn peek(input: &ParseBuffer<'a>) -> bool {
        let mut input = input.fork();
        input.skip_ignored();
        Self::matches(&input.tokens[0])
    }
    fn peek2(input: &ParseBuffer<'a>) -> bool {
        let mut input = input.fork();
        input.skip_ignored();
        input.skip(1);
        input.skip_ignored();
        Self::matches(&input.tokens[0])
    }
    fn peek3(input: &ParseBuffer<'a>) -> bool {
        let mut input = input.fork();
        input.skip_ignored();
        input.skip(1);
        input.skip_ignored();
        input.skip(1);
        input.skip_ignored();
        Self::matches(&input.tokens[0])
    }
}

pub struct Lookahead1<'a> {
    input: &'a ParseBuffer<'a>,
    expected: Vec<Expected>,
}

impl<'a> Lookahead1<'a> {
    #[inline]
    pub fn peek<T: Token<'a>>(&mut self) -> bool {
        self.expected.push(T::expected(self.input.start_span()));
        T::peek(self.input)
    }
    #[inline]
    pub fn error(self) {
        self.input.push_error(Error::Expected(self.expected))
    }
}

#[derive(Debug, Clone, Copy)]
pub struct DelimToken {
    pub delim: lexer::Delim,
    pub span: Span,
}

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

#[derive(Debug, Clone, Copy)]
pub struct PureIntToken {
    pub value: u32,
    pub span: Span,
}

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

#[derive(Debug, Clone, Copy)]
pub struct FloatToken {
    pub value: f64,
    pub ty: lexer::FloatType,
    pub span: Span,
}

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

#[derive(Debug, Clone)]
pub struct FmtStrToken {
    pub value: Arc<str>,
    pub span: Span,
}

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

#[derive(Debug, Clone)]
pub struct StrToken {
    pub value: Arc<str>,
    pub span: Span,
}

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

#[derive(Debug, Clone, Copy)]
pub struct CharToken {
    pub value: char,
    pub span: Span,
}

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

#[derive(Debug, Clone)]
pub struct LabelToken {
    pub name: Arc<str>,
    pub span: Span,
}

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

#[derive(Debug, Clone)]
pub struct AnyIdentToken {
    pub raw: bool,
    pub name: Arc<str>,
    pub span: Span,
}

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
    /// `keyword_token[mut]`: `mut` keyword
    MutToken[mut],
    /// `keyword_token[if]`: `if` keyword
    IfToken[if],
    /// `keyword_token[else]`: `if` keyword
    ElseToken[else],
}

#[derive(Debug, Clone, Copy)]
pub struct PunctToken {
    pub punct: Punct,
    pub span: Span,
}

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
    /// Grave Accent ```
    [Grave] => GraveToken[Grave] = "`",
    /// Reverse Solidus `\`
    [Bsol] => BsolToken[Bsol] = r"\",

    // multi-punct
}

#[derive(Debug, Clone, Copy)]
pub struct NewlineToken {
    pub newline: lexer::Newline,
    pub span: Span,
}

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

#[derive(Debug, Clone)]
pub struct InnerDocToken {
    pub value: Arc<str>,
    pub span: Span,
}

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
