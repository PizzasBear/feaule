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

mod char;
mod doc;
mod float;
mod group;
mod ident;
mod int;
mod label;
mod newline;
mod punct;
mod str;

pub use self::{
    char::*, doc::*, float::*, group::*, ident::*, int::*, label::*, newline::*, punct::*, str::*,
};

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

pub trait Spanned {
    fn span(&self) -> Span;
}

pub trait Parse<'a> {
    fn parse(input: &mut ParseBuffer<'a>) -> Self;
}

pub trait Token<'a>: Spanned {
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

impl<'a, T: Token<'a> + Parse<'a>> Parse<'a> for Option<T> {
    #[inline]
    fn parse(input: &mut ParseBuffer<'a>) -> Self {
        match T::peek(input) {
            true => Some(T::parse(input)),
            false => None,
        }
    }
}

macro_rules! impl_spanned {
    ($name:ident) => {
        impl Spanned for $name {
            #[inline]
            fn span(&self) -> Span {
                self.span
            }
        }
    };
}
use impl_spanned;
