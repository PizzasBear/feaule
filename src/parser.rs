use std::sync::Arc;

use itertools::Itertools;
use thiserror::Error;

use crate::{
    code::Span,
    lexer::{self, Delim},
};

pub mod buf;

use buf::{keyword_token, punct_token};

use self::buf::{IdentToken, ParenToken, Parse, ParseBuffer, PureIntToken, Spanned, Token};

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

#[derive(Debug, Clone)]
pub enum Member {
    Named(IdentToken),
    Unnamed(PureIntToken),
}

impl Spanned for Member {
    fn span(&self) -> Span {
        match self {
            Self::Named(tk) => tk.span(),
            Self::Unnamed(tk) => tk.span(),
        }
    }
}

impl Parse<'_> for Member {
    fn parse(input: &mut ParseBuffer<'_>) -> Self {
        let mut lookahead1 = input.lookahead1();
        if lookahead1.peek::<IdentToken>() {
            Self::Named(input.parse())
        } else if lookahead1.peek::<PureIntToken>() {
            Self::Unnamed(input.parse())
        } else {
            lookahead1.error();
            Self::Named(IdentToken {
                raw: false,
                name: "".into(),
                span: input.start_span(),
            })
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum PrefixOp {
    TakeRef(punct_token![&], Option<keyword_token![mut]>),
    Deref(punct_token![*]),
    Not(punct_token![!]),
    Neg(punct_token![-]),
}

#[derive(Debug)]
pub enum ExprValue {
    Int(u128, lexer::IntType),
    Float(f64, lexer::FloatType),
    Char(char),
    Str(Arc<str>),
    FStr(Arc<str>, Vec<(usize, Expr)>),
    Tuple(Vec<Expr>),
    MemberAccess(Box<Expr>, punct_token![.], Member),
    PrefixOp(PrefixOp, Box<Expr>),
}

#[derive(Debug)]
pub struct Expr {
    pub value: ExprValue,
    pub span: Span,
}

impl Expr {
    fn parse_ambiguous(input: &mut ParseBuffer) -> Self {
        todo!()
    }

    fn parse_subexpr(input: &mut ParseBuffer) -> Self {
        todo!();
    }

    fn parse_unary_expr(input: &mut ParseBuffer) -> Self {
        let start = input.start_span();
        let op = if let Some(tk) = input.parse() {
            Some(PrefixOp::TakeRef(tk, input.parse()))
        } else if let Some(tk) = input.parse() {
            Some(PrefixOp::Deref(tk))
        } else if let Some(tk) = input.parse() {
            Some(PrefixOp::Neg(tk))
        } else if let Some(tk) = input.parse() {
            Some(PrefixOp::Not(tk))
        } else {
            None
        };

        match op {
            Some(op) => {
                let value = Self::parse_unary_expr(input);
                Self {
                    span: start.merged_with(&value.span).unwrap(),
                    value: ExprValue::PrefixOp(op, Box::new(value)),
                }
            }
            None => todo!(),
        }
    }

    fn parse_trailer_expr(input: &mut ParseBuffer) -> Self {
        let mut expr = Self::parse_atom_expr(input);
        loop {
            if ParenToken::peek(input) {
                // TODO: impl fn call
                todo!();
            } else if <punct_token![.]>::peek(input) && !<punct_token![..]>::peek(input) {
                let mut dot: punct_token![.] = input.parse();
                let member: Member = input.parse();
                if ParenToken::peek(input) {
                    // TODO: impl method call
                    todo!();
                } else {
                    expr = Self {
                        span: expr.span.merged_with(&member.span()).unwrap(),
                        value: ExprValue::MemberAccess(Box::new(expr), dot, member),
                    };
                    continue;
                }
            } else {
                // TODO: impl rest
                todo!();
            }
        }
    }

    fn parse_atom_expr(input: &mut ParseBuffer) -> Self {
        todo!();
    }
}

impl Parse<'_> for Expr {
    fn parse(input: &mut ParseBuffer) -> Self {
        todo!();
    }
}
