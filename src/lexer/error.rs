use smallvec::{smallvec, SmallVec};

use crate::code::Span;

use super::Delim;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    // Comments
    #[error("Unexpected inner doc at {span:?}")]
    UnexpectedInnerDoc { span: Span },
    #[error("Unexpected outer doc at {span:?}")]
    UnexpectedOuterDoc { span: Span },
    #[error("Unclosed block comment over {span:?}")]
    UnclosedBlockComment { span: Span, open_span: Span },

    // Numbers
    #[error("Misplaced underscore in digits at {span:?}")]
    MisplacedUnderscoreInDigits { span: Span },
    #[error("Invalid base {base} at {span:?}")]
    InvalidDigitForBase { span: Span, base: u8 },
    #[error("Found a unicode digit at {span:?}, which isn't supported")]
    NonAsciiDigit { span: Span },
    #[error("Expected digits in base {base} at {span:?}")]
    ExpectedDigits { span: Span, base: u8 },
    #[error("Invalid base {base} for a floating point number used at {span:?}")]
    InvalidFloatBase { span: Span, base: u8 },
    #[error("Invalid suffix for a floating point number at {span:?}")]
    InvalidFloatSuffix { span: Span },
    #[error("Invalid suffix for a number at {span:?}")]
    InvalidNumberSuffix { span: Span },
    #[error(
        "Unmarked number suffix at {span:?} \
         (HINT: there should be an apostrophe (') between the number and the suffix, e.g. `300'f32`)"
    )]
    UnmarkedNumberSuffix { span: Span },
    #[error("Decimal float has hex power at {span:?} (HINT: try replacing `p` with `e`)")]
    DecimalFloatHasHexPower { span: Span },
    #[error("Floating point is so small that its parsed as zero at {span:?}")]
    AccidentalZeroFloat { span: Span },
    #[error("Numeric literal at {span:?} overflows its type")]
    OverflowingNumberLiteral { span: Span },

    // Group
    #[error("Illegal character {ch:?} at {span:?}")]
    IllegalCharacter { span: Span, ch: char },
    #[error("Mismatched delimeters {open_delim:?} at {open_span:?} and {close_delim:?} at {close_span:?}")]
    MismatchedDelimeters {
        open_delim: Delim,
        close_delim: Delim,
        open_span: Span,
        close_span: Span,
    },
    #[error("Unclosed group deliminated by {delim:?} over {span:?}")]
    UnclosedDelimeter {
        delim: Delim,
        open_span: Span,
        span: Span,
    },

    // String
    #[error("Unknown string flag {flag:?} at {span:?}")]
    UnknownStrFlag { span: Span, flag: char },
    #[error("Duplicate string flag {flag:?} at {span1:?} and {span2:?}")]
    DuplicateStrFlag {
        span1: Span,
        span2: Span,
        flag: char,
    },
    #[error("Unclosed string at {span:?}")]
    UnclosedStr { span: Span },

    // Char Literal
    #[error("Empty char literal at {span:?}")]
    EmptyCharLiteral { span: Span },
    #[error("Unclosed char literal at {span:?}")]
    UnclosedCharLiteral { span: Span },

    // Char
    #[error(r"Out of range ascii escape, it must be in the range '\x00' - '\x7f'")]
    OutOfRangeAsciiEscape { span: Span },
    #[error("Unknown character escape {escape:?} at {span:?}")]
    UnknownCharEscape { span: Span, escape: char },
    #[error(r"Ascii escape is too short at {span:?} (it should look like '\x61')")]
    AsciiCharEscapeTooShort { span: Span },
    #[error("Incorrect unicode escape sequence at {span:?}")]
    IncorrectUnicodeEscapeSequence { span: Span },
    #[error("Invalid character {ch:?} in unicode escape at {span:?}")]
    InvalidCharacterInUnicodeEscape { span: Span, ch: char },
    #[error("Unterminated unicode escape at {span:?}")]
    UnterminatedUnicodeEscape { span: Span },
    #[error("Overly long unicode escape at {span:?}, must have at most 6 digits")]
    OverlongUnicodeEscape { span: Span },
    #[error("Invalid unicode character escape at {span:?}")]
    InvalidUnicodeCharacterEscape { span: Span, escape_val: u32 },
}

impl Error {
    pub fn spans(&self) -> SmallVec<[&Span; 2]> {
        match &self {
            // Comments
            Self::UnexpectedInnerDoc { span } => smallvec![span],
            Self::UnexpectedOuterDoc { span } => smallvec![span],
            Self::UnclosedBlockComment { span, .. } => smallvec![span],

            // Numbers
            Self::MisplacedUnderscoreInDigits { span } => smallvec![span],
            Self::InvalidDigitForBase { span, .. } => smallvec![span],
            Self::NonAsciiDigit { span, .. } => smallvec![span],
            Self::ExpectedDigits { span, .. } => smallvec![span],
            Self::InvalidFloatBase { span, .. } => smallvec![span],
            Self::InvalidFloatSuffix { span } => smallvec![span],
            Self::InvalidNumberSuffix { span } => smallvec![span],
            Self::UnmarkedNumberSuffix { span } => smallvec![span],
            Self::DecimalFloatHasHexPower { span } => smallvec![span],
            Self::AccidentalZeroFloat { span } => smallvec![span],
            Self::OverflowingNumberLiteral { span } => smallvec![span],

            // Group
            Self::IllegalCharacter { span, .. } => smallvec![span],
            Self::MismatchedDelimeters {
                open_span,
                close_span,
                ..
            } => smallvec![open_span, close_span],
            Self::UnclosedDelimeter { span, .. } => smallvec![span],

            // String
            Self::UnknownStrFlag { span, .. } => smallvec![span],
            Self::DuplicateStrFlag { span1, span2, .. } => smallvec![span1, span2],
            Self::UnclosedStr { span } => smallvec![span],

            // Char Literal
            Self::EmptyCharLiteral { span } => smallvec![span],
            Self::UnclosedCharLiteral { span } => smallvec![span],

            // Char
            Self::OutOfRangeAsciiEscape { span } => smallvec![span],
            Self::UnknownCharEscape { span, .. } => smallvec![span],
            Self::AsciiCharEscapeTooShort { span, .. } => smallvec![span],
            Self::IncorrectUnicodeEscapeSequence { span, .. } => smallvec![span],
            Self::InvalidCharacterInUnicodeEscape { span, .. } => smallvec![span],
            Self::UnterminatedUnicodeEscape { span } => smallvec![span],
            Self::OverlongUnicodeEscape { span } => smallvec![span],
            Self::InvalidUnicodeCharacterEscape { span, .. } => smallvec![span],
        }
    }
}
