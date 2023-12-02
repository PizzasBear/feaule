use std::{fmt, num::NonZeroU8, str, sync::Arc};

use crate::code::Span;

macro_rules! def_punct {
    ($dollar:tt $($(#[$attr:meta])* $name:ident[$punct:tt] = $lit:literal),+ $(,)?) => {
        #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
        #[non_exhaustive]
        #[repr(u8)]
        pub enum Punct {
            End = 0,
            $(
                $(#[$attr])*
                $name = $lit,
            )+
        }

        impl Punct {
            pub const CHARS: &'static str =
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

        #[macro_export]
        macro_rules! punct {
            [End] => { $crate::lexer::Punct::End };
            $([$punct] => { $crate::lexer::Punct::$name };)+
            [$dollar($dollar tt:tt),+ $dollar (,)?] => { [$dollar($crate::lexer::punct! $dollar tt),+] }
        }
        pub use punct;
    };
}

def_punct! { $
    /// Plus Sign `+`
    Plus[+] = b'+',
    /// Minus Sign `-`
    Minus[-] = b'-',
    /// Asterisk `*`
    Ast[*] = b'*',
    /// Solidus `/`
    Sol[/] = b'/',
    /// Percent Sign `%`
    Percnt[%] = b'%',
    /// Exclamation Mark `!`
    Excl[!] = b'!',
    /// Ampersand `&`
    Amp[&] = b'&',
    /// Vertical Line `|`
    Verbar[|] = b'|',
    /// Circumflex Accent `^`
    Hat[^] = b'^',
    /// Tilde Symbol `~`
    Tilde[~] = b'~',
    /// Dollar Sign `$`
    Dollar[$] = b'$',
    /// Commercial At Symbol `@`
    Commat[@] = b'@',
    /// Less Than Sign `<`
    Lt[<] = b'<',
    /// Greater Than Sign `>`
    Gt[>] = b'>',
    /// Equals Sign `=`
    Equals[=] = b'=',
    /// Full Stop `.`
    Period[.] = b'.',
    /// Comma `,`
    Comma[,] = b',',
    /// Colon `:`
    Colon[:] = b':',
    /// Semicolon `;`
    Semi[;] = b';',
    /// Question Mark `?`
    Quest[?] = b'?',
    /// Grave Accent ```
    Grave[Grave] = b'`',
    /// Reverse Solidus `\`
    Bsol[Bsol] = b'\\',
}

impl fmt::Debug for Punct {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_end() {
            write!(f, "punct![End]")
        } else {
            write!(f, "punct![{}]", char::from_u32(*self as _).unwrap())
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
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

impl Default for IntType {
    #[inline]
    fn default() -> Self {
        Self::Auto
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FloatType {
    Auto,
    F64,
    F32,
}

impl Default for FloatType {
    #[inline]
    fn default() -> Self {
        Self::Auto
    }
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

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Int {
    pub value: u128,
    pub is_pure: bool,
    pub ty: IntType,
}

impl Default for Int {
    #[inline]
    fn default() -> Self {
        Self {
            value: 0,
            is_pure: true,
            ty: IntType::Auto,
        }
    }
}

impl fmt::Debug for Int {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        const MAX_PURE: u128 = u32::MAX as _;
        match self {
            Self {
                value: value @ 0..=MAX_PURE,
                is_pure: true,
                ty: IntType::Auto,
            } => write!(f, "PureInt({value:?})",),
            Self {
                value,
                is_pure: true,
                ty,
            } => write!(f, "FakePureInt({value:?}, {ty:?})",),
            Self {
                value,
                is_pure: false,
                ty,
            } => write!(f, "ImpureInt({value:?}, {ty:?})",),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Default)]
pub struct Float {
    pub value: f64,
    pub ty: FloatType,
}

impl fmt::Debug for Float {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self { value, ty } = self;
        write!(f, "Float({value:?}, {ty:?})")
    }
}

pub struct FmtStrArg {
    pub index: usize,
    pub span: Span,
    pub tokens: Vec<TokenTree>,
}

impl fmt::Debug for FmtStrArg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "fmt_arg![at {} containing ", self.index)?;
        fmt::Debug::fmt(self.tokens.as_slice(), f)?;
        write!(f, " @ {:?}]", self.span)?;
        Ok(())
    }
}

#[derive(Debug)]
pub struct FmtStr {
    pub value: Arc<str>,
    pub args: Vec<FmtStrArg>,
}

pub enum TokenTreeValue {
    Float(Float),
    Int(Int),
    // BigInt(BigUint),
    Str(Arc<str>),
    FmtStr(FmtStr),
    Char(char),
    Label(Arc<str>),
    Ident { raw: bool, ident: Arc<str> },
    Punct(Punct),
    Newline(Newline),
    InnerDoc(Arc<str>),
    OuterDoc(Arc<str>),
    Group(Delim, Vec<TokenTree>),
}

impl fmt::Debug for TokenTreeValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int(int) => write!(f, "{int:?}"),
            Self::Float(float) => write!(f, "{float:?}"),
            Self::Str(s) => write!(f, "Str({s:?})"),
            Self::FmtStr(fmt_s) => write!(f, "{fmt_s:?}"),
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

pub struct TokenTree {
    pub value: TokenTreeValue,
    pub span: Span,
}

impl fmt::Debug for TokenTree {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "token![")?;
        fmt::Debug::fmt(&self.value, f)?;
        write!(f, " @ {:?}]", self.span)?;
        Ok(())
    }
}
