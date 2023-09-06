use std::{fmt, num::NonZeroU8, str};

use crate::code::Span;

macro_rules! def_punct {
    ($($name:ident $lit:literal),+ $(,)?) => {
        #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
        #[non_exhaustive]
        #[repr(u8)]
        pub enum Punct {
            End = 0,
            $(
                $name = $lit,
            )+
        }

        impl Punct {
            pub const CHARS: &str =
                match ::std::str::from_utf8(&[$($lit),+]) {
                    Ok(s) => {
                        let mut i = 0;
                        while i < s.len() {
                            assert!(s.as_bytes()[i].is_ascii(), "Failed to convert into ascii string");
                            i += 1;
                        }
                        s
                    }
                    Err(_) => panic!("Failed to convert into utf8 string")
                };

            pub const fn from_ascii(ch: u8) -> Option<Self> {
                match ch {
                    $($lit => Some(Self::$name),)+
                    _ => None
                }
            }
        }
    };
}

def_punct!(
    Plus b'+', Minus b'-', Ast b'*', Sol b'/', Percnt b'%', Excl b'!', Amp b'&',
    Verbar b'|', Circ b'^', Tilde b'~', Dollar b'$', Commat b'@', Lt b'<', Gt b'>',
    Equals b'=', Period b'.', Comma b',', Colon b':', Semi b';', Quest b'?',
    Grave b'`', Bsol b'\\',
);

impl fmt::Debug for Punct {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_end() {
            write!(f, "Punct::End")
        } else {
            write!(f, "Punct({:?})", char::from_u32(*self as _).unwrap())
        }
    }
}

impl Punct {
    #[inline]
    pub const fn is_end(self) -> bool {
        matches!(self, Self::End)
    }

    #[inline]
    pub const fn get_char(self) -> Option<char> {
        match self.get() {
            Some(ch) => char::from_u32(ch.get() as _),
            None => None,
        }
    }

    #[inline]
    pub const fn get(self) -> Option<NonZeroU8> {
        NonZeroU8::new(self as _)
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
        value: u128,
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
