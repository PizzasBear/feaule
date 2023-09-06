use crate::{
    code::Span,
    lexer::{Punct, Token, TokenValue},
};

type Result<T, E = Error> = std::result::Result<T, E>;
pub enum Error {
    UnexpectedToken,
}

#[derive(Debug)]
pub enum ExprValue {}

#[derive(Debug)]
pub struct Expr {
    pub value: ExprValue,
    pub span: Span,
}

#[derive(Debug, Clone, Copy)]
pub enum PrefixOp {
    Deref,
    TakeRef,
    Not,
    Neg,
    Pos,
}

fn parse_prefix_ops(
    tokens: &[Token],
    mut pos: usize,
    _errors: &mut Vec<Error>,
) -> (usize, Vec<(Span, PrefixOp)>) {
    let mut result = vec![];
    loop {
        let tk = &tokens[pos];
        match tk.value {
            TokenValue::Punct(punct) => {
                let op = match punct {
                    Punct::Ast => PrefixOp::Deref,
                    Punct::Amp => PrefixOp::TakeRef,
                    Punct::Minus => PrefixOp::Neg,
                    Punct::Plus => PrefixOp::Pos,
                    Punct::Excl => PrefixOp::Not,
                    _ => break,
                };
                result.push((tk.span, op));
                pos += 1;
            }
            _ => break,
        }
    }
    (pos, result)
}

pub fn parse_value(tokens: &[Token], mut pos: usize, errors: &mut Vec<Error>) -> (usize, Expr) {
    let _prefix_ops;
    (pos, _prefix_ops) = parse_prefix_ops(tokens, pos, errors);

    match tokens[pos].value {
        TokenValue::Int { value, ty, .. } => todo!(),
        TokenValue::Float(value, ty) => todo!(),

        _ => {
            errors.push(Error::UnexpectedToken);
        }
    }

    _ = pos;

    todo!()
}

pub fn parse_expr(_tokens: &[Token], _pos: usize, _errors: &mut Vec<Error>) -> (usize, Expr) {
    todo!();
}

pub fn parse_file(_tokens: &[Token]) -> () {
    todo!()
}
