use std::sync::Arc;

use itertools::Itertools;
use thiserror::Error;

use crate::{
    code::Span,
    lexer::{self, Delim, Punct, TokenTree, TokenTreeValue},
};

pub mod buf;

use buf::{keyword_token, punct_token};

type Result<T, E = Error> = std::result::Result<T, E>;

#[derive(Debug, Error)]
pub enum Expected {
    #[error("an integer at {span:?}")]
    Int { span: Span },
    #[error("a pure integer at {span:?}")]
    PureInt { span: Span },
    #[error("a float at {span:?}")]
    Float { span: Span },
    #[error("a string at {span:?}")]
    Str { span: Span },
    #[error("a character literal at {span:?}")]
    Char { span: Span },
    #[error("a format string at {span:?}")]
    FmtStr { span: Span },
    #[error("any identifier at {span:?}")]
    AnyIdent { span: Span },
    #[error("the keyword `{kw}` at {span:?}")]
    Keyword { span: Span, kw: &'static str },
    #[error("a non-reserved identifier at {span:?}")]
    Ident { span: Span },
    #[error("a label at {span:?}")]
    Label { span: Span },
    #[error("any punctuation at {span:?}")]
    AnyPunct { span: Span },
    #[error("the punctuation {punct:?} at {span:?}")]
    Punct { punct: &'static str, span: Span },
    #[error("a newline at {span:?}")]
    Newline { span: Span },
    #[error("inner documentation at {span:?}")]
    InnerDoc { span: Span },
    #[error("outer documentation at {span:?}")]
    OuterDoc { span: Span },
    #[error("a group deliminated by {delim:?} at {span:?}")]
    Delim { delim: Delim, span: Span },
    #[error("a group at {span:?}")]
    Group { span: Span },
}

#[derive(Debug, Error)]
pub enum Error {
    #[error("Unexpected token at {span:?}")]
    UnexpectedToken { span: Span },
    #[error("Expected{} {}",
        if 1 < .0.len() { " either" } else { "" },
        .0.iter().format(" or "),
    )]
    Expected(Vec<Expected>),
}

#[derive(Debug)]
pub enum ExprValue {
    Int(u128, lexer::IntType),
    Float(f64, lexer::FloatType),
    Char(char),
    Str(Arc<str>),
    FStr(Arc<str>, Vec<(usize, Expr)>),
    Tuple(Vec<Expr>),
    FieldAccess(Box<Expr>, Arc<str>),
}

#[derive(Debug)]
pub struct Expr {
    pub value: ExprValue,
    pub span: Span,
}

#[derive(Debug, Clone, Copy)]
pub enum PrefixOp {
    Deref(punct_token![*]),
    TakeRef(punct_token![&]),
    TakeMut(punct_token![&], keyword_token![mut]),
    Not(punct_token![!]),
    Neg(punct_token![-]),
    Pos(punct_token![+]),
}

macro_rules! tkpat {
    (@value Float $tt:tt $(| $($or:tt)+)? $(,)?) => { TokenTreeValue::Float(lexer::Float $tt) $(| tkpat!(@value $($or)+))? };
    (@value Int $tt:tt $(| $($or:tt)+)? $(,)?) => { TokenTreeValue::Int(lexer::Int $tt) $(| tkpat!(@value $($or)+))? };
    (@value Str $tt:tt $(| $($or:tt)+)? $(,)?) => { TokenTreeValue::Str $tt $(| tkpat!(@value $($or)+))? };
    (@value FStr $tt:tt $(| $($or:tt)+)? $(,)?) => { TokenTreeValue::FmtStr(lexer::FmtStr $tt) $(| tkpat!(@value $($or)+))? };
    (@value Char $tt:tt $(| $($or:tt)+)? $(,)?) => { TokenTreeValue::Char $tt $(| tkpat!(@value $($or)+))? };
    (@value Label $tt:tt $(| $($or:tt)+)? $(,)?) => { TokenTreeValue::Label $tt $(| tkpat!(@value $($or)+))? };
    (@value Ident $tt:tt $(| $($or:tt)+)? $(,)?) => { TokenTreeValue::Ident $tt $(| tkpat!(@value $($or)+))? };
    (@value Punct::$punct:ident $(| $($or:tt)+)? $(,)?) => { TokenTreeValue::Punct(Punct::$punct) $(| tkpat!(@value $($or)+))? };
    (@value Punct $tt:tt $(| $($or:tt)+)? $(,)?) => { TokenTreeValue::Punct $tt $(| tkpat!(@value $($or)+))? };
    (@value Newline $tt:tt $(| $($or:tt)+)? $(,)?) => { TokenTreeValue::Newline $tt $(| tkpat!(@value $($or)+))? };
    (@value InnerDoc $tt:tt $(| $($or:tt)+)? $(,)?) => { TokenTreeValue::InnerDoc $tt $(| tkpat!(@value $($or)+))? };
    (@value OuterDoc $tt:tt $(| $($or:tt)+)? $(,)?) => { TokenTreeValue::OuterDoc $tt $(| tkpat!(@value $($or)+))? };
    (@value Group $tt:tt $(| $($or:tt)+)? $(,)?) => { TokenTreeValue::Group $tt $(| tkpat!(@value $($or)+))? };
    (@value $pat:pat_param $(| $($or:tt)+)? $(,)?) => { $pat $(| tkpat!(@value $($or)+))? };
    ($span:pat, $($value:tt)+) => {
        TokenTree {
            value: tkpat!(@value $($value)+),
            span: $span,
        }
    };
    ($span:pat $(,)?) => {
        TokenTree {
            span: $span,
            ..
        }
    };
}

macro_rules! tkpats {
    () => { [] };
    (@part $pat:pat) => { $pat };
    (@part {$($tt:tt)*}) => { tkpat!($($tt)*) };
    ($($($part:tt)|+),+ $(,)?) => { [$($(tkpats!(@part $part))|+),+] }
}

pub fn parse_value(tokens: &[TokenTree], mut pos: usize, errors: &mut Vec<Error>) -> (usize, Expr) {
    let mut prefix_ops = vec![];
    while let Some(tk) = tokens.get(pos) {
        match tk.value {
            TokenTreeValue::Punct(punct) => {
                let op = match punct {
                    Punct::Ast => PrefixOp::Deref,
                    Punct::Amp => match tokens.get(pos + 1) {
                        Some(TokenTree {
                            value: TokenTreeValue::Ident { raw: false, ident },
                            ..
                        }) if &**ident == Keyword::Mut.as_str() => {
                            pos += 1;
                            PrefixOp::TakeMut
                        }
                        _ => PrefixOp::TakeRef,
                    },
                    Punct::Minus => PrefixOp::Neg,
                    Punct::Plus => PrefixOp::Pos,
                    Punct::Excl => PrefixOp::Not,
                    _ => break,
                };
                prefix_ops.push((tk.span, op));
                pos += 1;
            }
            _ => break,
        }
    }

    let mut value = match tokens.get(pos) {
        Some(&tkpat!(span, Int { value, ty, .. })) => Expr {
            value: ExprValue::Int(value, ty),
            span,
        },
        Some(&tkpat!(span, Float { value, ty })) => Expr {
            value: ExprValue::Float(value, ty),
            span,
        },
        Some(&tkpat!(span, Char(ch))) => Expr {
            value: ExprValue::Char(ch),
            span,
        },
        Some(&tkpat!(span, Str(ref s))) => Expr {
            value: ExprValue::Str(s.clone()),
            span,
        },
        Some(&tkpat!(span, FStr { ref value, ref args })) => Expr {
            value: ExprValue::FStr(
                value.clone(),
                args.iter()
                    .map(|lexer::FmtStrArg { index, tokens, .. }| {
                        (*index, parse_expr(tokens, 0, errors).1)
                    })
                    .collect(),
            ),
            span,
        },
        tk => {
            let span = match tk {
                None => tokens.last().unwrap().span.end_span(),
                Some(tk) => tk.span,
            };
            errors.push(Error::UnexpectedToken { span });
            Expr {
                value: ExprValue::Tuple(vec![]),
                span: span.start_span(),
            }
        }
    };

    loop {
        match tokens[pos..] {
            tkpats![{_, Punct::Period}, {span, Ident { raw: _, ref ident }}, ..] => {
                pos += 2;
                value = Expr {
                    span: value.span.merged_with(&span).unwrap(),
                    value: ExprValue::FieldAccess(Box::new(value), ident.clone()),
                };
            }
            _ => break,
        }
    }

    _ = pos;

    todo!()
}

pub fn parse_expr(_tokens: &[TokenTree], _pos: usize, _errors: &mut Vec<Error>) -> (usize, Expr) {
    todo!();
}

pub fn parse_file(_tokens: &[TokenTree]) -> () {
    todo!()
}
