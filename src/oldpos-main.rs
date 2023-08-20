use std::{fmt, ops, path::Path, str};

use lazy_static::lazy_static;
use num::{BigInt, BigUint, Zero};
use regex::{Regex, RegexSet};
use smallvec::{smallvec, SmallVec};

pub const INNER_LINE_DOC_START: &str = "#!";
pub const INNER_BLOCK_DOC_START: &str = "<#!";

pub const OUTER_LINE_DOC_START: &str = "##";
pub const OUTER_BLOCK_DOC_START: &str = "<##";

pub const LINE_COMMENT_START: &str = "#";
pub const BLOCK_COMMENT_START: &str = "<#";

pub const BLOCK_COMMENT_END: &str = "#>";

pub const WHITESPACE_CHARS: &str = " \t\r";
pub const SPECIAL_CHARS: &str = r"#()[]{}\";
pub const PUNCT_CHARS: &str = "+-*/%,!&|^~$@<>=.,:;?'`";

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct Pos(Span);

impl Default for Pos {
    fn default() -> Self {
        Self::new()
    }
}

impl Pos {
    #[inline]
    pub const fn new() -> Self {
        Self(Span::zero())
    }

    #[inline]
    pub const fn idx(&self) -> usize {
        self.0.idx()
    }

    #[inline]
    pub const fn line(&self) -> u32 {
        self.0.line()
    }

    #[inline]
    pub const fn column(&self) -> u32 {
        self.0.column()
    }

    /// Equivilent to calling `advance_str` with an ascii string with length `num_bytes` that doesn't contain any newlines (`'\n'`).
    /// Important that the underlying string doesn't contain newlines and is ascii because otherwise `pos.column()` and/or `pos.line()`
    /// will break.
    ///
    /// # Example
    /// ```
    /// let code = "hello world";
    ///
    /// let mut pos1 = Pos::new();
    /// pos1.advance_ascii_offset("hello".len());
    ///
    /// let mut pos2 = Pos::new();
    /// pos2.advance_str("hello");
    ///
    /// assert_eq!(pos1, pos2);
    /// ```
    #[inline]
    pub fn advance_ascii_offset(&mut self, num_bytes: usize) {
        self.0.idx += num_bytes as u32;
        self.0.column += num_bytes as u32;
    }

    #[inline]
    pub fn advance_newline(&mut self) {
        self.0.line += 1;
        self.0.column = 1;
        self.0.idx += 1;
    }

    #[inline]
    pub fn advance(&mut self, ch: char) {
        if ch == '\n' {
            self.0.column = 0;
            self.0.line += 1;
        }
        self.0.column += 1;
        self.0.idx += ch.len_utf8() as u32;
    }

    pub fn advance_str(&mut self, s: &str) {
        lazy_static! {
            static ref NEWLINE: Regex = Regex::new(r"\n").unwrap();
        }

        let idx;
        if let Some((lines, mch)) = NEWLINE.find_iter(s).enumerate().last() {
            self.0.line += lines as u32;
            self.0.column = 1;
            idx = mch.end();
        } else {
            idx = 0;
        }

        self.0.column += s[idx..].chars().count() as u32;
        self.0.idx += s.len() as u32;
    }

    #[inline]
    pub const fn with_len(self, len: usize) -> Span {
        Span {
            len: len as _,
            ..self.0
        }
    }

    /// Creates a span from this position with `len` non-newline ascii characters backwards.
    ///
    /// # Example
    /// ```
    /// let code = "hello world";
    ///
    /// let mut pos = Pos::new();
    /// let span = pos.with_len(5);
    /// pos.advance_ascii_offset(5);
    /// assert_eq!(span, pos.with_backwards_ascii_len(5));
    /// ```
    #[inline]
    pub const fn with_backwards_ascii_len(self, len: usize) -> Span {
        Span {
            len: len as _,
            column: self.0.column - len as u32,
            ..self.0
        }
    }

    #[inline]
    pub const fn to(self, other: &Self) -> Span {
        self.with_len(other.idx() - self.idx())
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(C, align(8))]
pub struct Span {
    // pub start: u32,
    // pub end: u32,
    // pub buf: u32,
    pub idx: u32,
    pub len: u32,
    pub line: u32,
    pub column: u32,
}

impl Span {
    #[inline]
    pub const fn zero() -> Self {
        Self {
            idx: 0,
            len: 0,
            line: 1,
            column: 1,
        }
    }

    #[inline]
    pub const fn start(self) -> Pos {
        Pos(Span { len: 0, ..self })
    }

    #[inline]
    pub const fn idx(&self) -> usize {
        self.idx as _
    }

    #[inline]
    pub const fn end_idx(&self) -> usize {
        self.idx() + self.len()
    }

    #[inline]
    pub const fn range(&self) -> ops::Range<usize> {
        self.idx()..self.end_idx()
    }

    #[inline]
    pub const fn len(&self) -> usize {
        self.len as _
    }

    #[inline]
    pub const fn line(&self) -> u32 {
        self.line as _
    }

    #[inline]
    pub const fn column(&self) -> u32 {
        self.column as _
    }

    #[inline]
    pub const fn merged_with(&self, other: &Self) -> Self {
        #[inline]
        const fn max(a: usize, b: usize) -> u32 {
            if a < b {
                b as _
            } else {
                a as _
            }
        }

        if other.idx < self.idx {
            other.merged_with(self)
        } else {
            Self {
                len: max(self.end_idx(), other.end_idx()) as u32 - self.idx,
                ..*self
            }
        }
    }

    #[inline]
    pub const fn merged_with_pos(&self, pos: &Pos) -> Self {
        #[inline]
        const fn max(a: u32, b: u32) -> u32 {
            if a < b {
                b
            } else {
                a
            }
        }

        if self.idx() < pos.idx() {
            Self {
                len: max(self.len, pos.0.idx - self.idx),
                ..*self
            }
        } else {
            Self {
                len: pos.0.idx - self.idx + self.len,
                ..pos.0
            }
        }
    }

    pub fn show<P: AsRef<Path>>(&self, code: &str, path: &P) {
        fn find_line_start(code: &str, pos: &Pos) -> usize {
            if pos.line() == 1 {
                0
            } else if pos.column() == 1 {
                pos.idx()
            } else {
                code[..pos.idx()].rfind('\n').map_or(0, |i| i + 1)
            }
        }

        fn find_line_end(code: &str, pos: &Pos) -> usize {
            code[pos.idx()..].find('\n').map_or(code.len(), |i| i + 1)
        }

        let path = path.as_ref();

        let mut end = self.start();
        end.advance_str(&code[self.range()]);
        let off = end.line().ilog10() as usize + 1;

        println!("{:off$}--> {path:?}:{}:{}", "", self.line(), self.column());
        match end.line() - self.line() {
            0 => println!(
                "{} | {}",
                self.line(),
                &code[find_line_start(code, &self.start())..find_line_end(code, &end)],
            ),
            1..=4 => todo!(),
            5.. => todo!(),
        }
    }
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {
            idx,
            len,
            line,
            column,
        } = *self;
        write!(f, "Span({idx}({line}:{column}),{len})")
    }
}

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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FormatAlign {
    Left,
    Center,
    Right,
    Fill,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FormatSign {
    /// `'+'` option - adds a plus sign in front of positive numbers
    /// (e.g. `"+42", "-42"`)
    ExplicitPositive,
    /// `'-'` option - doesn't put anything in front of positive numbers
    /// (e.g. `"42", "-42"`)
    ImplicitPositive,
    /// `' '` option - adds space in front of positive numbers
    /// (e.g. `" 42", "-42"`)
    AlignedPositive,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FormatGrouping {
    /// `','` option - Comma thousands separator and dot fractional marker (e.g. `1,123,456.7890123`)
    CommaDot,
    /// `'_'` option - Underscore thousands separator and dot fractional marker (e.g. `1_123_456.7890123`)
    UnderscoreDot,
    /// `'.'` option - Dot thousands separator and comma fractional marker (e.g. `1.123.456,7890123`)
    DotComma,
    /// `' '` option - Space thousands separator and comma fractional marker (e.g. `1 123 456,7890123`)
    SpaceComma,
    /// no option - No thousands seperator and dot fractional marker (e.g. `1123456.7890123`)
    NoneDot,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FormatType {
    /// Base 2 numbers (e.g. 107 turns into `1101011`)
    Binary,
    /// Base 3 numbers (e.g. 107 turns into `10222`)
    Ternary,
    /// Base 4 numbers (e.g. 107 turns into `1223`)
    Quaternary,
    /// Base 6 numbers (e.g. 107 turns into `265`)
    Seximal,
    /// Base 8 numbers (e.g. 107 turns into `153`)
    Octal,
    /// Base 10 numbers (e.g. 107 turns into `107`)
    Decimal,
    /// Lowercase base 12 numbers (e.g. 107 turns into `8b`)
    LowerDozenal,
    /// Uppercase base 12 numbers (e.g. 107 turns into `8B`)
    UpperDozenal,
    /// Lowercase base 16 numbers (e.g. 107 turns into `6b`)
    LowerHex,
    /// Uppercase base 16 numbers (e.g. 107 turns into `6B`)
    UpperHex,
    /// Lowercase scientific numbers (e.g. 107 turns into `1.07e+02`)
    LowerScientific,
    /// Uppercase scientific numbers (e.g. 107 turns into `1.07E+02`)
    UpperScientific,
}

#[derive(Clone, PartialEq, Eq)]
pub struct FormatSpec {
    fill: char,
    align: FormatAlign,
    sign: FormatSign,
    alternate: bool,
    width: u32,
    grouping: FormatGrouping,
    grouping_freq: SmallVec<[u8; 23]>,
    percision: Option<u32>,
    ty: FormatType,
    debug: bool,
}

impl fmt::Debug for FormatSpec {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "fmt\"")?;
        let fill_escape = self.fill.escape_debug();
        let align_ch = match self.align {
            FormatAlign::Left => '<',
            FormatAlign::Center => '^',
            FormatAlign::Right => '>',
            FormatAlign::Fill => '=',
        };
        let alternate_s = if self.alternate { "#" } else { "" };
        let sign_ch = match self.sign {
            FormatSign::ExplicitPositive => '+',
            FormatSign::ImplicitPositive => '-',
            FormatSign::AlignedPositive => ' ',
        };
        write!(f, "{fill_escape}{align_ch}{alternate_s}{sign_ch}")?;
        if self.grouping != FormatGrouping::NoneDot {
            let ch = match self.grouping {
                FormatGrouping::DotComma => '.',
                FormatGrouping::CommaDot => ',',
                FormatGrouping::UnderscoreDot => '_',
                FormatGrouping::SpaceComma => ' ',
                FormatGrouping::NoneDot => unreachable!(),
            };
            write!(f, "g{ch}",)?;
            for &v in &self.grouping_freq {
                write!(f, "{v}")?;
            }
        }
        if let Some(v) = self.percision {
            write!(f, ".{v}")?;
        }
        let ty_ch = match self.ty {
            FormatType::Binary => 'b',
            FormatType::Ternary => 't',
            FormatType::Quaternary => 'q',
            FormatType::Seximal => 's',
            FormatType::Octal => 'o',
            FormatType::Decimal => 'd',
            FormatType::LowerDozenal => 'z',
            FormatType::UpperDozenal => 'Z',
            FormatType::LowerHex => 'x',
            FormatType::UpperHex => 'X',
            FormatType::LowerScientific => 'e',
            FormatType::UpperScientific => 'E',
        };
        let dbg_s = if self.debug { "?" } else { "" };
        write!(f, "{ty_ch}{dbg_s}\"")?;

        Ok(())
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
    FStr(String, Vec<(FormatSpec, Vec<Token>)>),
    Char(char),
    Label(String),
    Ident(String),
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
            Self::Ident(s) => write!(f, "Ident({s:?})"),
            Self::Punct(punct) => fmt::Debug::fmt(punct, f),
            Self::Newline(Newline) => write!(f, "Newline"),
            Self::InnerDoc(s) => write!(f, "InnerDoc({s:?})"),
            Self::OuterDoc(s) => write!(f, "OuterDoc({s:?})"),
            Self::Group(delim, tokens) => {
                write!(f, "group![{delim:?} of ")?;
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

#[derive(Debug)]
pub enum LexError {
    // Comments
    UnexpectedInnerDoc {
        span: Span,
    },
    UnexpectedOuterDoc {
        span: Span,
    },
    UnclosedBlockComment {
        span: Span,
        open_span: Span,
    },

    // Numbers
    MisplacedUnderscoreInDigits {
        span: Span,
    },
    InvalidDigitForBase {
        span: Span,
        base: u8,
    },
    NonAsciiDigit {
        span: Span,
    },
    ExpectedDigits {
        span: Span,
        base: u8,
    },
    InvalidFloatBase {
        span: Span,
        base: u8,
    },
    InvalidFloatSuffix {
        span: Span,
    },
    InvalidNumberSuffix {
        span: Span,
    },
    UnmarkedNumberSuffix {
        span: Span,
    },
    DecimalFloatHasHexPower {
        span: Span,
    },
    AccidentalZeroFloat {
        span: Span,
    },
    OverflowingLiteral {
        span: Span,
    },
    MismatchedDelimeters {
        open_delim: Delim,
        close_delim: Delim,
        open_span: Span,
        close_span: Span,
    },
    UnclosedDelimeter {
        delim: Delim,
        open_span: Span,
        close_span: Span,
    },

    // String
    UnknownStrFlag {
        span: Span,
        flag: char,
    },
    DuplicateStrFlag {
        span1: Span,
        span2: Span,
        flag: char,
    },

    // Char
    OutOfRangeAsciiEscape {
        span: Span,
    },
    UnknownCharEscape {
        span: Span,
        escape: char,
    },
    AsciiCharEscapeTooShort {
        span: Span,
    },
    IncorrectUnicodeEscapeSequence {
        span: Span,
        ch: char,
    },
}

fn unindent(out: &mut String, s: &str) {
    let lines: SmallVec<[_; 32]> = s
        .split('\n')
        .map(|line| {
            let trimmed = line.trim_start_matches(&[' ', '\t', '\r']);
            (
                line.len() - trimmed.len(),
                line.trim_end_matches(&[' ', '\t', '\r']),
            )
        })
        .collect();

    let Some(&(_, first_line)) = lines.first() else {
        return;
    };

    let indent = lines[1..]
        .iter()
        .filter_map(|&(indent, line)| {
            if indent < line.len() {
                Some(indent)
            } else {
                None
            }
        })
        .min()
        .unwrap_or(0);

    if !out.is_empty() && !out.ends_with('\n') {
        out.push('\n');
    }

    if !first_line.is_empty() {
        out.push_str(first_line);
    }

    let mut push_newline = !first_line.is_empty();
    for &(line_indent, line) in &lines[1..] {
        if push_newline {
            out.push('\n');
        } else {
            push_newline = true;
        }
        if line_indent < line.len() {
            out.push_str(&line[indent..]);
        }
    }
}

pub fn lex_whitespace(
    code: &str,
    mut pos: Pos,
    tokens: &mut Vec<Token>,
    errors: &mut Vec<LexError>,
    allow_outer_doc: bool,
    mut allow_inner_doc: bool,
) -> Pos {
    allow_inner_doc &= tokens.is_empty();

    let mut newline = None;
    let mut outer_doc = None;
    let mut inner_doc = None;
    while pos.idx() < code.len() {
        for &ch in &code.as_bytes()[pos.idx()..] {
            if ch == b'\n' {
                if newline.is_none() {
                    newline = Some(Token {
                        span: pos.with_len(1),
                        value: TokenValue::Newline(Newline),
                    })
                }

                pos.advance_newline();
            } else if WHITESPACE_CHARS.as_bytes().contains(&ch) {
                pos.advance_ascii_offset(1);
            } else {
                break;
            }
        }

        lazy_static! {
            static ref START: RegexSet = RegexSet::new(&[
                &format!("^{}", regex::escape(INNER_BLOCK_DOC_START)),
                &format!("^{}", regex::escape(OUTER_BLOCK_DOC_START)),
                &format!("^{}", regex::escape(BLOCK_COMMENT_START)),
                &format!("^{}", regex::escape(INNER_LINE_DOC_START)),
                &format!("^{}", regex::escape(OUTER_LINE_DOC_START)),
                &format!("^{}", regex::escape(LINE_COMMENT_START)),
            ])
            .unwrap();
            static ref BLOCK_DEPTH_MOD: Regex = Regex::new(&format!(
                "({})|{}|{}|{}",
                regex::escape(BLOCK_COMMENT_END),
                regex::escape(INNER_BLOCK_DOC_START),
                regex::escape(OUTER_BLOCK_DOC_START),
                regex::escape(BLOCK_COMMENT_START),
            ))
            .unwrap();
        }

        let start_pos = pos;

        let Some(block_type) = START.matches(&code[pos.idx()..]).into_iter().next() else {
            break;
        };

        if matches!(block_type, 0 | 1) && newline.is_none() {
            newline = Some(Token {
                span: pos.with_len(0),
                value: TokenValue::Newline(Newline),
            });
        }

        pos.advance_ascii_offset(match block_type {
            0 => INNER_BLOCK_DOC_START.len(),
            1 => OUTER_BLOCK_DOC_START.len(),
            2 => BLOCK_COMMENT_START.len(),
            3 => INNER_LINE_DOC_START.len(),
            4 => OUTER_LINE_DOC_START.len(),
            5 => LINE_COMMENT_START.len(),
            _ => unreachable!(),
        });

        if code[pos.idx()..].starts_with(' ') {
            pos.advance_ascii_offset(1);
        }

        let text_start_pos = pos;
        let text_end_pos;

        match block_type {
            0 | 1 | 2 => {
                let mut depth = 1u32;
                let mut end_range = code.len()..code.len();
                for cap in BLOCK_DEPTH_MOD.captures_iter(&code[pos.idx()..]) {
                    if cap.get(1).is_some() {
                        depth -= 1;
                        if depth == 0 {
                            let range = cap.get(0).unwrap().range();
                            end_range = pos.idx() + range.start..pos.idx() + range.end;
                            break;
                        }
                    } else {
                        depth += 1;
                    }
                }
                pos.advance_str(&code[pos.idx()..end_range.start]);
                text_end_pos = pos;
                pos.advance_ascii_offset(end_range.len());

                if end_range.len() == 0 {
                    errors.push(LexError::UnclosedBlockComment {
                        span: start_pos.to(&pos),
                        open_span: start_pos.to(&text_start_pos),
                    });
                    break;
                }
            }
            3 | 4 | 5 => {
                let i = code.as_bytes()[pos.idx()..]
                    .iter()
                    .enumerate()
                    .find_map(|(i, &ch)| if ch == b'\n' { Some(i) } else { None })
                    .unwrap_or(code.len());

                pos.advance_ascii_offset(i);
                if newline.is_none() {
                    newline = Some(Token {
                        span: pos.with_len(1),
                        value: TokenValue::Newline(Newline),
                    })
                }
                pos.advance_newline();
                text_end_pos = pos;
            }
            _ => unreachable!(),
        }

        let content = &code[text_start_pos.idx()..text_end_pos.idx()];
        let total_span = start_pos.to(&pos);

        match block_type {
            0 | 3 => {
                if allow_inner_doc {
                    let (spans, doc) = inner_doc.get_or_insert((vec![], String::new()));
                    unindent(doc, content);
                    spans.push(total_span);
                } else {
                    match errors.last_mut() {
                        Some(LexError::UnexpectedInnerDoc { span }) => {
                            *span = total_span;
                        }
                        _ => errors.push(LexError::UnexpectedInnerDoc { span: total_span }),
                    }
                }
            }
            1 | 4 => {
                allow_inner_doc = false;

                if allow_outer_doc {
                    let (spans, doc) = outer_doc.get_or_insert((vec![], String::new()));
                    unindent(doc, content);
                    spans.push(total_span);
                } else {
                    match errors.last_mut() {
                        Some(LexError::UnexpectedOuterDoc { span }) => {
                            *span = total_span;
                        }
                        _ => errors.push(LexError::UnexpectedOuterDoc { span: total_span }),
                    }
                }
            }
            2 | 5 => {}
            _ => unreachable!(),
        }
    }

    if let Some((spans, inner_doc)) = inner_doc {
        tokens.push(Token {
            value: TokenValue::InnerDoc(inner_doc),
            span: *spans.last().unwrap(),
        });
    } else if let Some(newline) = newline {
        tokens.push(newline);
    }

    if let Some((spans, outer_doc)) = outer_doc {
        tokens.push(Token {
            value: TokenValue::OuterDoc(outer_doc),
            span: *spans.last().unwrap(),
        });
    }
    pos
}

pub fn parse_digits(
    code: &str,
    mut pos: Pos,
    base: u32,
    allow_no_digits: bool,
    errors: &mut Vec<LexError>,
) -> (Pos, SmallVec<[u8; 23]>, bool) {
    lazy_static! {
        static ref UNDERSCORES: Regex = Regex::new("^_*").unwrap();
    }

    let start_pos = pos;
    let mut is_pure = true;

    let mut num_underscores = UNDERSCORES.find(&code[pos.idx()..]).unwrap().len();
    pos.advance_ascii_offset(num_underscores);
    is_pure &= num_underscores == 0;
    if 0 < num_underscores {
        errors.push(LexError::MisplacedUnderscoreInDigits {
            span: pos.with_backwards_ascii_len(num_underscores),
        });
    }
    let mut digits = smallvec![];
    while pos.idx() < code.len() {
        match code[pos.idx()..].chars().next() {
            Some(ch) if ch.is_digit(base) => {
                let digit = ch.to_digit(base).unwrap();
                digits.push(digit as _);

                pos.advance(ch);
            }
            Some(ch) if ch.is_ascii_digit() => {
                errors.push(LexError::InvalidDigitForBase {
                    span: pos.with_len(ch.len_utf8()),
                    base: base as _,
                });

                pos.advance_ascii_offset(ch.len_utf8());
            }
            Some(ch) if ch.is_numeric() => {
                errors.push(LexError::NonAsciiDigit {
                    span: pos.with_len(ch.len_utf8()),
                });

                pos.advance(ch);
            }
            _ if num_underscores == 1 => {
                errors.push(LexError::MisplacedUnderscoreInDigits {
                    span: pos.with_backwards_ascii_len(num_underscores),
                });
                break;
            }
            _ => break,
        }

        num_underscores = UNDERSCORES.find(&code[pos.idx()..]).unwrap().len();
        pos.advance_ascii_offset(num_underscores);
        is_pure &= num_underscores == 0;
        if 1 < num_underscores {
            errors.push(LexError::MisplacedUnderscoreInDigits {
                span: pos.with_backwards_ascii_len(num_underscores),
            });
        }
    }

    if digits.is_empty() && !allow_no_digits {
        errors.push(LexError::ExpectedDigits {
            span: start_pos.to(&pos),
            base: base as _,
        });
    }

    (pos, digits, is_pure)
}

pub fn lex_number(
    code: &str,
    mut pos: Pos,
    tokens: &mut Vec<Token>,
    errors: &mut Vec<LexError>,
) -> Pos {
    let start_pos = pos;

    let mut is_pure = true;

    let base = 'base: {
        let base = match &code.as_bytes()[pos.idx()..pos.idx() + 2] {
            b"0x" | b"0X" => 16,
            b"0z" | b"0Z" => 12,
            b"0d" | b"0D" => 10,
            b"0o" | b"0O" => 8,
            b"0s" | b"0S" => 6,
            b"0q" | b"0Q" => 4,
            b"0t" | b"0T" => 3,
            b"0b" | b"0B" => 2,
            _ => break 'base 10,
        };

        is_pure = false;
        pos.advance_ascii_offset(2);
        base
    };

    let (whole_digits, tmp_is_pure);
    (pos, whole_digits, tmp_is_pure) = parse_digits(code, pos, base, false, errors);
    is_pure &= tmp_is_pure;

    let frac_digits = if code[pos.idx()..].starts_with('.') {
        is_pure = false;
        pos.advance_ascii_offset(1);

        let frac_digits;
        (pos, frac_digits, _) = parse_digits(code, pos, base, true, errors);
        Some(frac_digits)
    } else {
        None
    };

    let power_digits = match code[pos.idx()..].chars().next() {
        Some(ch @ ('e' | 'p')) => {
            is_pure = false;
            pos.advance_ascii_offset(1);

            match (ch, base) {
                ('e', 10) | ('p', 16) => {}
                ('p', 10) => {
                    errors.push(LexError::DecimalFloatHasHexPower {
                        span: pos.with_backwards_ascii_len(1),
                    });
                }
                ('e', 16) => unreachable!(),
                (_, _) => {
                    // TODO: Bad float base
                }
            }

            let neg = match code[pos.idx()..].chars().next() {
                Some(ch @ ('+' | '-')) => {
                    pos.advance_ascii_offset(1);
                    ch == '-'
                }
                _ => false,
            };
            let unsigned_power_digits;
            (pos, unsigned_power_digits, _) = parse_digits(code, pos, 10, false, errors);
            Some((neg, unsigned_power_digits))
        }
        _ => None,
    };

    #[derive(Debug)]
    enum NumberSuffix {
        Float(FloatType),
        Int(IntType),
        Unknown,
    }

    lazy_static! {
        static ref SUFFIX: Regex = Regex::new(r"^(')?(\w+)").unwrap();
    }

    let suffix = SUFFIX.captures(&code[pos.idx()..]).map(|c| {
        let suffix = c.get(2).unwrap().as_str();

        if c.get(1).is_some() {
            pos.advance_ascii_offset(1);
        }
        let suffix_start = pos;
        pos.advance_str(suffix);

        let suffix_span = suffix_start.to(&pos);

        if c.get(1).is_none() {
            errors.push(LexError::UnmarkedNumberSuffix { span: suffix_span })
        }

        (
            match suffix {
                "uptr" => NumberSuffix::Int(IntType::UPtr),
                "u128" => NumberSuffix::Int(IntType::U128),
                "u64" => NumberSuffix::Int(IntType::U64),
                "u32" => NumberSuffix::Int(IntType::U32),
                "u16" => NumberSuffix::Int(IntType::U16),
                "u8" => NumberSuffix::Int(IntType::U8),

                "iptr" => NumberSuffix::Int(IntType::IPtr),
                "i128" => NumberSuffix::Int(IntType::I128),
                "i64" => NumberSuffix::Int(IntType::I64),
                "i32" => NumberSuffix::Int(IntType::I32),
                "i16" => NumberSuffix::Int(IntType::I16),
                "i8" => NumberSuffix::Int(IntType::I8),

                "f64" => NumberSuffix::Float(FloatType::F64),
                "f32" => NumberSuffix::Float(FloatType::F32),

                _ => NumberSuffix::Unknown,
            },
            suffix_span,
        )
    });

    if frac_digits.is_some()
        || power_digits.is_some()
        || matches!(suffix, Some((NumberSuffix::Float(_), _)))
    {
        let frac_digits = frac_digits.unwrap_or_else(|| smallvec![0]);
        let (is_power_neg, power_digits) = power_digits.unwrap_or_else(|| (false, smallvec![0]));
        let ty = match suffix {
            Some((NumberSuffix::Float(ty), _)) => ty,
            Some((_, span)) => {
                errors.push(LexError::InvalidFloatSuffix { span });
                FloatType::Auto
            }
            None => FloatType::Auto,
        };

        match base {
            10 => {
                let mut bytes = SmallVec::<[u8; 128]>::new();
                bytes.extend(whole_digits.iter().map(|&d| b'0' + d));
                bytes.push(b'.');
                bytes.extend(frac_digits.iter().map(|&d| b'0' + d));
                bytes.push(b'e');
                if is_power_neg {
                    bytes.push(b'-');
                }
                bytes.extend(power_digits.iter().map(|&d| b'0' + d));

                let value = unsafe { str::from_utf8_unchecked(&bytes) }
                    .parse::<f64>()
                    .unwrap();

                tokens.push(Token {
                    value: TokenValue::Float(value, ty),
                    span: start_pos.to(&pos),
                });
            }
            16 => {
                let mut power = BigInt::from_radix_be(
                    if is_power_neg {
                        num::bigint::Sign::Minus
                    } else {
                        num::bigint::Sign::Plus
                    },
                    &power_digits,
                    10,
                )
                .unwrap()
                    - 4 * frac_digits.len();

                let mut all_frac_digits = whole_digits;
                all_frac_digits.extend_from_slice(&frac_digits);

                let mut frac = BigUint::from_radix_be(&all_frac_digits, 16).unwrap();

                println!("{frac}*2^{power}");
                let value = if !frac.is_zero() {
                    // 0b1101p+0
                    // 0b1.101p+3

                    power += frac.bits() as i64 - 1;
                    if frac.bits() < 53 {
                        frac <<= 53 - frac.bits();
                    } else if 53 < frac.bits() {
                        let b = frac.bit(frac.bits() - 54);
                        frac >>= frac.bits() - 53;
                        if b {
                            frac += 1u32;
                        }
                    }

                    let mut frac = u64::try_from(frac).unwrap();

                    match (power.sign(), i32::try_from(&power)) {
                        (_, Ok(power @ -1022..=1023)) => {
                            f64::from_bits(((power + 1023) as u64) << 52 | frac & (1 << 52) - 1)
                        }
                        (_, Ok(power @ -1074..=-1023)) => {
                            let shift = -1022 - power;
                            frac = (frac >> shift) + (1 & frac >> shift - 1);
                            f64::from_bits(frac)
                        }
                        (_, Ok(-1075)) if (frac >> 51 == 1) => f64::from_bits(1),
                        (num::bigint::Sign::Minus, _) => {
                            errors.push(LexError::AccidentalZeroFloat {
                                span: start_pos.to(&pos),
                            });
                            0.0
                        }
                        (num::bigint::Sign::Plus, _) => {
                            errors.push(LexError::OverflowingLiteral {
                                span: start_pos.to(&pos),
                            });
                            f64::INFINITY
                        }
                        (num::bigint::Sign::NoSign, _) => unreachable!(),
                    }
                } else {
                    0.0
                };

                tokens.push(Token {
                    value: TokenValue::Float(value, ty),
                    span: start_pos.to(&pos),
                });
            }
            _ => {
                errors.push(LexError::InvalidFloatBase {
                    span: start_pos.to(&pos),
                    base: base as _,
                });

                tokens.push(Token {
                    value: TokenValue::Float(1.0, ty),
                    span: start_pos.to(&pos),
                });
            }
        }
    } else {
        let ty = match suffix {
            Some((NumberSuffix::Int(ty), _)) => ty,
            Some((NumberSuffix::Float(_), _)) => unreachable!(),
            Some((NumberSuffix::Unknown, span)) => {
                errors.push(LexError::InvalidNumberSuffix { span });
                IntType::Auto
            }
            None => IntType::Auto,
        };

        tokens.push(Token {
            value: TokenValue::Int {
                value: BigUint::from_radix_be(&whole_digits, base).unwrap(),
                is_pure,
                ty,
            },
            span: start_pos.to(&pos),
        });
    }

    pos
}

pub fn parse_char_escape(
    code: &str,
    mut pos: Pos,
    errors: &mut Vec<LexError>,
) -> (Pos, Option<char>) {
    let start_pos = pos;

    match code[pos.idx()..].chars().next() {
        Some('\\') => {
            pos.advance('\\');

            match code[pos.idx()..].chars().next() {
                Some(ch @ ('a' | 'b' | 'f' | 'n' | 'r' | 't' | 'v' | '0' | '\\' | '\'' | '"')) => {
                    pos.advance_ascii_offset(1);
                    let ch = match ch {
                        'a' => '\x07',
                        'b' => '\x08',
                        'f' => '\x0c',
                        'n' => '\n',
                        'r' => '\r',
                        't' => '\t',
                        'v' => '\x0b',
                        '0' => '\0',
                        '\\' => '\\',
                        '\'' => '\'',
                        '"' => '"',
                        _ => unreachable!(),
                    };
                    (pos, Some(ch))
                }
                Some('x') => {
                    pos.advance_ascii_offset(1);
                    let Some(dg1) = code[pos.idx()..]
                        .chars()
                        .next()
                        .and_then(|ch| ch.to_digit(16))
                    else {
                        errors.push(LexError::AsciiCharEscapeTooShort {
                            span: start_pos.to(&pos),
                        });
                        return (pos, Some('\0'));
                    };
                    pos.advance_ascii_offset(1);

                    let Some(dg2) = code[pos.idx()..]
                        .chars()
                        .next()
                        .and_then(|ch| ch.to_digit(16))
                    else {
                        errors.push(LexError::AsciiCharEscapeTooShort {
                            span: start_pos.to(&pos),
                        });
                        return (pos, Some(char::from_u32(dg1).unwrap()));
                    };
                    pos.advance_ascii_offset(1);

                    if 7 < dg2 {
                        errors.push(LexError::OutOfRangeAsciiEscape {
                            span: pos.with_backwards_ascii_len(2),
                        });
                    }

                    (pos, Some(char::from_u32(16 * dg1 + dg2).unwrap()))
                }
                Some('u') => {
                    pos.advance_ascii_offset(1);

                    if !code[pos.idx()..].starts_with('{') {
                        errors.push(LexError::IncorrectUnicodeEscapeSequence {
                            span: start_pos.to(&pos),
                            ch: '{',
                        });
                        return (pos, Some('\0'));
                    }
                    pos.advance_ascii_offset(1);

                    let digits;
                    (pos, digits, _) = parse_digits(code, pos, 16, false, errors);

                    match code[pos.idx()..].chars().next() {
                        Some('}') => {}
                        Some(ch) => {
                            errors.push(LexError::InvalidCharacterInUnicodeEscape {
                                span: pos.with_len(ch.len_utf8()),
                                ch,
                            });
                        }
                        None => {
                            errors.push(LexError::UnterminatedUnicodeEscape {
                                span: start_pos.to(&pos),
                            });
                        }
                    }

                    if 6 < digits.len() {
                        todo!();
                    }

                    let v = digits
                        .iter()
                        .rev()
                        .enumerate()
                        .try_fold(0, |v, (i, &d)| {
                            Some(v | (d as u32).checked_shl(4 * i as _)?)
                        })
                        .unwrap_or_else(|| {
                            errors.push(LexError::OverlongUnicodeEscape {
                                span: start_pos.to(&pos),
                            });
                            0
                        });
                    // InvalidUnicodeCharacterEscape
                    let ch = char::from_u32(v).unwrap_or_else(|| {
                        errors.push(LexError::InvalidUnicodeCharacterEscape {
                            span: start_pos.to(&pos),
                        });
                        '\0'
                    });
                    (pos, Some(ch))
                    // // r"incorrect unicode escape sequence: expected `\u{...}`"
                    // chars.take_char('{').map_err(|_| {
                    //     Error::IncorrectUnicodeEscapeSequence(chars.span_from(file.clone(), start))
                    // })?;

                    // let (unicode_len, unicode) = parse_bigint(&mut chars, 16);
                    // chars.take_char('}').map_err(|err| match err {
                    //     TakeCharError::NoChar => {
                    //         return Err(Error::UnterminatedUnicodeEscape(
                    //             chars.span_from(file.clone(), start),
                    //         ))
                    //     } // "unterminated unicode escape: missing a closing `}`"
                    //     TakeCharError::BadChar(ch) => {
                    //         return Err(Error::InvalidCharacterInUnicodeEscape {
                    //             ch,
                    //             span: chars.span_from(file.clone(), start),
                    //         })
                    //     } // "invalid character in unicode escape: `{}`",
                    // });

                    // match unicode_len {
                    //     // "empty unicode escape: this escape must have at least 1 hex digit"
                    //     1..=6 => {
                    //         let unicode: u32 = unicode.try_into().unwrap();
                    //         let ch = char::from_u32(unicode).ok_or_else(|| {
                    //             Error::InvalidUnicodeCharacterEscape {
                    //                 escape: unicode,
                    //                 span: chars.span_from(file.clone(), start),
                    //             }
                    //         })?;
                    //         // "invalid unicode character escape"
                    //         value.push(ch);
                    //     }
                    //     0 => {
                    //         return Err(Error::EmptyUnicodeEscape(
                    //             chars.span_from(file.clone(), start),
                    //         ));
                    //     }
                    //     _ => {
                    //         return Err(Error::OverlongUnicodeEscape(
                    //             chars.span_from(file.clone(), start),
                    //         ))
                    //     } // "overlong unicode escape: must have at most 6 hex digits"
                    // }
                }
                Some(ch) => {
                    pos.advance(ch);
                    errors.push(LexError::UnknownCharEscape {
                        escape: ch,
                        span: start_pos.to(&pos),
                    });
                    (pos, Some(ch))
                }
                None => {
                    // errors.push()
                    todo!();
                }
            }
        }
        Some(ch) => {
            pos.advance(ch);
            (pos, Some(ch))
        }
        None => (pos, None),
    }
}

/// Must start with the regex: `^(?:[\w&&\D]\w*)?"`
pub fn lex_str(
    code: &str,
    mut pos: Pos,
    tokens: &mut Vec<Token>,
    errors: &mut Vec<LexError>,
) -> Pos {
    lazy_static! {
        static ref STR_START_DELIM: Regex = Regex::new(r#"^"*"#).unwrap();
        static ref FORMAT_SPEC: Regex = Regex::new(
            r#"(?x)
                ^
                (?:
                    (?P<fill> \\\$ | \\\{ | \\\\ | [^\{\$\\] )?
                    (?P<align> [<\^=>])
                )?
                (?P<sign> [+-\ ])?
                (?P<alt> #)?
                (?P<width> [0-9]+)?
                (?:
                    g
                    (?P<grouping> [\.,_\ ])
                    (?P<group_freq>[1-9]+)?
                )?
                (?: . (?P<percision> [0-9]+) )?
                (?P<type> [btqsodzZxXeEfF])?
                (?P<debug> \?)?
            "#
        )
        .unwrap();
        static ref PROC_POS: Regex = Regex::new(r#" \$ | \\ | "+ "#).unwrap();
    }

    let start_pos = pos;

    let mut raw_flag_span = None;
    let mut multiline_flag_span = None;
    let mut format_flag_span = None;

    for ch in code[pos.idx()..].chars() {
        let flag_span = match ch {
            'r' => Some(&mut raw_flag_span),
            'm' => Some(&mut multiline_flag_span),
            'f' => Some(&mut format_flag_span),

            '"' => break,
            ch => {
                errors.push(LexError::UnknownStrFlag {
                    span: pos.with_len(ch.len_utf8()),
                    flag: ch,
                });
                None
            }
        };
        match flag_span {
            Some(&mut Some(span1)) => errors.push(LexError::DuplicateStrFlag {
                span1,
                span2: pos.with_len(1),
                flag: ch,
            }),
            Some(flag_span @ None) => *flag_span = Some(pos.with_len(1)),
            None => {}
        }
        pos.advance(ch);
    }

    let delim_len = STR_START_DELIM
        .find(&code[pos.idx()..])
        .unwrap()
        .as_str()
        .len();
    pos.advance_ascii_offset(delim_len);

    if delim_len == 2 {
        tokens.push(Token {
            value: match format_flag_span {
                Some(_) => TokenValue::FStr(String::new(), vec![]),
                None => TokenValue::Str(String::new()),
            },
            span: start_pos.to(&pos),
        })
    } else {
        let mut result = String::new();
        let mut fmt_args: Vec<()> = vec![];

        if 2 < delim_len {
            if code[pos.idx()..].starts_with(r#"\""#) {
                pos.advance_ascii_offset(2);
                result.push('"');
            } else if code[pos.idx()..].starts_with(r#"\\""#) {
                pos.advance_ascii_offset(2);
                result.push('\\');
            }
        }

        loop {
            if let Some(mch) = PROC_POS.find(&code[pos.idx()..]) {
                let s = &code[pos.idx()..pos.idx() + mch.start()];
                pos.advance_str(s);
                result.push_str(s);

                let seg_start_pos = pos;
                pos.advance_ascii_offset(mch.len());

                match mch.as_str() {
                    "$" => {
                        if format_flag_span.is_some() {
                            todo!();
                        } else {
                            result.push('$');
                        }
                    }
                    "\\" => {
                        if raw_flag_span.is_some() {
                            result.push('\\');
                        } else {
                            todo!();
                        }
                    }
                    closer => {
                        if closer.len() < delim_len {
                            result.push_str(closer);
                        } else {
                            result.push_str(&closer[delim_len..]);
                            break;
                        }
                    }
                }
            } else {
                // Unclosed string
                todo!()
            }
        }
    }

    pos
}

pub fn lex_group(
    code: &str,
    mut pos: Pos,
    tokens: &mut Vec<Token>,
    errors: &mut Vec<LexError>,
    opening_delim: Delim,
    opening_delim_span: Span,
) -> Pos {
    while pos.idx() < code.len() {
        pos = lex_whitespace(code, pos, tokens, errors, true, true);

        if code.len() <= pos.idx() {
            debug_assert_eq!(pos.idx(), code.len());
            break;
        }

        lazy_static! {
            static ref TST_STR: Regex = Regex::new(r"^[^ \t\r\n#]+").unwrap();
            static ref START_TY: Regex = Regex::new(
                r#"(?x)
                    ^(?P<num>\d)
                      | (?P<group>[\(\[\{\}\]\)])
                      | (?P<str>(?:[\w&&\D]\w*)?")
                      | (?P<ident>(?:r')?[\w&&\D]\w*)
                      | (?P<any>.)
                "#
            )
            .unwrap();
        }

        let ty = START_TY.captures(&code[pos.idx()..]).unwrap();

        if ty.name("num").is_some() {
            pos = lex_number(code, pos, tokens, errors);
        } else if let Some(mch) = ty.name("group") {
            let start_pos = pos;
            pos.advance_ascii_offset(1);
            let tk_delim = match mch.as_str() {
                "(" | ")" => Delim::Paren,
                "[" | "]" => Delim::Bracket,
                "{" | "}" => Delim::Brace,
                _ => unreachable!(),
            };
            match mch.as_str() {
                "(" | "[" | "{" => {
                    let mut tk_tokens = vec![];
                    pos = lex_group(
                        code,
                        pos,
                        &mut tk_tokens,
                        errors,
                        tk_delim,
                        pos.with_backwards_ascii_len(1),
                    );
                    tokens.push(Token {
                        value: TokenValue::Group(tk_delim, tk_tokens),
                        span: start_pos.to(&pos),
                    });
                }
                ")" | "]" | "}" => {
                    if tk_delim != opening_delim {
                        errors.push(LexError::MismatchedDelimeters {
                            open_delim: opening_delim,
                            close_delim: tk_delim,

                            open_span: opening_delim_span,
                            close_span: pos.with_backwards_ascii_len(1),
                        });
                    }
                    return pos;
                }
                _ => unreachable!(),
            }
        } else if ty.name("str").is_some() {
            todo!()
        } else if let Some(mch) = ty.name("ident") {
            tokens.push(Token {
                value: TokenValue::Ident(mch.as_str().to_owned()),
                span: pos.with_len(mch.len()),
            });

            pos.advance_str(mch.as_str());
        } else if let Some(mch) = ty.name("any") {
            let ch = mch.as_str().chars().next().unwrap();
            tokens.push(Token {
                value: TokenValue::Char(ch),
                span: pos.with_len(ch.len_utf8()),
            });

            pos.advance(ch);
        }
    }

    if opening_delim != Delim::None {
        errors.push(LexError::UnclosedDelimeter {
            delim: opening_delim,
            open_span: opening_delim_span,

            close_span: pos.with_len(0),
        });
    }

    pos
}

pub fn lex(code: &str) -> (Vec<Token>, Vec<LexError>) {
    let mut pos = Pos::new();
    let mut errors = vec![];
    let mut tokens = vec![];

    while pos.idx() < code.len() {
        pos = lex_group(
            code,
            pos,
            &mut tokens,
            &mut errors,
            Delim::None,
            pos.with_len(0),
        );
    }

    (tokens, errors)
}

fn main() {
    tracing_subscriber::fmt()
        .with_max_level(tracing::Level::INFO)
        .init();

    let code = std::fs::read_to_string("test.txt").unwrap();

    let (tokens, errors) = lex(&code);
    // `a {hello} big` => ("My name is", "max", "and")
    println!("tokens = {tokens:#?}");
    println!("errors = {errors:#?}");
}

// fn f() {
//     println!("Does something!");
