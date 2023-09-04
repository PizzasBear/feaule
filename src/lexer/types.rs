use core::fmt;

use num::BigUint;

use crate::code::Span;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Punct(u8);

impl fmt::Debug for Punct {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_end() {
            write!(f, "Punct::END")
        } else {
            write!(f, "Punct({:?})", char::from_u32(self.0 as _).unwrap())
        }
    }
}

impl Punct {
    pub const END: Self = Self::end();

    #[inline]
    pub const fn new(ch: u8) -> Self {
        Self(ch)
    }

    #[inline]
    pub const fn end() -> Self {
        Self(0)
    }

    #[inline]
    pub const fn is_end(&self) -> bool {
        self.0 == 0
    }

    #[inline]
    pub const fn get(&self) -> Option<u8> {
        if self.is_end() {
            None
        } else {
            Some(self.0)
        }
    }
}

#[derive(Debug)]
pub struct Newline;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IntType {
    Auto,
    UPtr,
    U128,
    U64,
    U32,
    U16,
    U8,
    IPtr,
    I128,
    I64,
    I32,
    I16,
    I8,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FloatType {
    Auto,
    F64,
    F32,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Delim {
    /// No delimiter `/* start */ ... /* end */`
    None,
    /// Parenthesis `(...)`
    Paren,
    /// Square brackets `[...]`
    Bracket,
    /// Curly braces `{ ... }`
    Brace,
}

impl fmt::Debug for Delim {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::None => write!(f, "None"),
            Self::Paren => write!(f, "Paren()"),
            Self::Bracket => write!(f, "Bracket[]"),
            Self::Brace => write!(f, "Brace{{}}"),
        }
    }
}

pub enum TokenValue {
    Float(f64, FloatType),
    Int {
        value: BigUint,
        is_pure: bool,
        ty: IntType,
    },
    // BigInt(BigUint),
    Str(String),
    FStr(String, Vec<(usize, Token)>),
    Char(char),
    Label(String),
    Ident {
        raw: bool,
        ident: String,
    },
    Punct(Punct),
    Newline(Newline),
    InnerDoc(String),
    OuterDoc(String),
    Group(Delim, Vec<Token>),
}

impl fmt::Debug for TokenValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Float(x, ty) => write!(f, "Float({x:?}, {ty:?})"),
            Self::Int {
                value,
                is_pure: true,
                ty: IntType::Auto,
            } => write!(f, "PureInt({value:?})",),
            Self::Int {
                value,
                is_pure: true,
                ty,
            } => write!(f, "FakePureInt({value:?}, {ty:?})",),
            Self::Int {
                value,
                is_pure: false,
                ty,
            } => write!(f, "ImpureInt({value:?}, {ty:?})",),
            Self::Str(s) => write!(f, "Str({s:?})"),
            Self::FStr(s, tokens) => f.debug_tuple("FStr").field(s).field(tokens).finish(),
            Self::Char(ch) => write!(f, "Char({ch:?})"),
            Self::Label(s) => write!(f, "Label({s:?})"),
            &Self::Ident { raw, ref ident } => {
                write!(f, "Ident(\"{}{ident}\")", if raw { "r'" } else { "" })
            }
            Self::Punct(punct) => fmt::Debug::fmt(punct, f),
            Self::Newline(Newline) => write!(f, "Newline"),
            Self::InnerDoc(s) => write!(f, "InnerDoc({s:?})"),
            Self::OuterDoc(s) => write!(f, "OuterDoc({s:?})"),
            Self::Group(delim, tokens) => {
                write!(f, "group![{delim:?} containing ")?;
                fmt::Debug::fmt(tokens.as_slice(), f)?;
                write!(f, "]")?;
                Ok(())
            }
        }
    }
}

pub struct Token {
    pub value: TokenValue,
    pub span: Span,
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "token![")?;
        fmt::Debug::fmt(&self.value, f)?;
        write!(f, " @ {:?}]", self.span)
    }
}
