use std::str;

use lazy_static::lazy_static;
use num::{BigInt, BigUint, Zero};
use regex::{Regex, RegexSet};
use smallvec::{smallvec, SmallVec};

use crate::code::{Pos, Span};

use super::{types::*, Error};

pub const INNER_LINE_DOC_START: &str = "#!";
pub const INNER_BLOCK_DOC_START: &str = "<#!";

pub const OUTER_LINE_DOC_START: &str = "##";
pub const OUTER_BLOCK_DOC_START: &str = "<##";

pub const LINE_COMMENT_START: &str = "#";
pub const BLOCK_COMMENT_START: &str = "<#";

pub const BLOCK_COMMENT_END: &str = "#>";

pub const WHITESPACE_CHARS: &str = " \t\r";
// pub const SPECIAL_CHARS: &str = r"#()[]{}\";
pub const PUNCT_CHARS: &str = r"+-*/%,!&|^~$@<>=.,:;?'`\";

fn unindent(out: &mut String, s: &str) {
    let lines: SmallVec<[_; 32]> = s
        .split('\n')
        .map(|line| {
            let trimmed = line.trim_start_matches(|ch| WHITESPACE_CHARS.contains(ch));
            (
                line.len() - trimmed.len(),
                line.trim_end_matches(|ch| WHITESPACE_CHARS.contains(ch)),
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
    errors: &mut Vec<Error>,
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

                pos.advance_by(1);
            } else if WHITESPACE_CHARS.as_bytes().contains(&ch) {
                pos.advance_by(1);
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

        pos.advance_by(match block_type {
            0 => INNER_BLOCK_DOC_START.len(),
            1 => OUTER_BLOCK_DOC_START.len(),
            2 => BLOCK_COMMENT_START.len(),
            3 => INNER_LINE_DOC_START.len(),
            4 => OUTER_LINE_DOC_START.len(),
            5 => LINE_COMMENT_START.len(),
            _ => unreachable!(),
        });

        if code[pos.idx()..].starts_with(' ') {
            pos.advance_by(1);
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
                pos.go_to(end_range.start);
                text_end_pos = pos;
                pos.advance_by(end_range.len());

                if end_range.len() == 0 {
                    errors.push(Error::UnclosedBlockComment {
                        span: start_pos.to(pos).unwrap(),
                        open_span: start_pos.to(text_start_pos).unwrap(),
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

                pos.advance_by(i);
                if newline.is_none() {
                    newline = Some(Token {
                        span: pos.with_len(1),
                        value: TokenValue::Newline(Newline),
                    })
                }
                pos.advance_by(1);
                text_end_pos = pos;
            }
            _ => unreachable!(),
        }

        let content = &code[text_start_pos.idx()..text_end_pos.idx()];
        let total_span = start_pos.to(pos).unwrap();

        match block_type {
            0 | 3 => {
                if allow_inner_doc {
                    let (spans, doc) = inner_doc.get_or_insert((vec![], String::new()));
                    unindent(doc, content);
                    spans.push(total_span);
                } else {
                    match errors.last_mut() {
                        Some(Error::UnexpectedInnerDoc { span }) => {
                            *span = total_span;
                        }
                        _ => errors.push(Error::UnexpectedInnerDoc { span: total_span }),
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
                        Some(Error::UnexpectedOuterDoc { span }) => {
                            *span = total_span;
                        }
                        _ => errors.push(Error::UnexpectedOuterDoc { span: total_span }),
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
    errors: &mut Vec<Error>,
) -> (Pos, SmallVec<[u8; 23]>, bool) {
    lazy_static! {
        static ref UNDERSCORES: Regex = Regex::new("^_*").unwrap();
    }

    let start_pos = pos;
    let mut is_pure = true;

    let mut num_underscores = UNDERSCORES.find(&code[pos.idx()..]).unwrap().len();
    pos.advance_by(num_underscores);
    is_pure &= num_underscores == 0;
    if 0 < num_underscores {
        errors.push(Error::MisplacedUnderscoreInDigits {
            span: pos.with_backwards_len(num_underscores),
        });
    }
    let mut digits = smallvec![];
    while pos.idx() < code.len() {
        match code[pos.idx()..].chars().next() {
            Some(ch) if ch.is_digit(base) => {
                let digit = ch.to_digit(base).unwrap();
                digits.push(digit as _);

                pos.advance_by(ch.len_utf8());
            }
            Some(ch) if ch.is_ascii_digit() => {
                errors.push(Error::InvalidDigitForBase {
                    span: pos.with_len(ch.len_utf8()),
                    base: base as _,
                });

                pos.advance_by(ch.len_utf8());
            }
            Some(ch) if ch.is_numeric() => {
                errors.push(Error::NonAsciiDigit {
                    span: pos.with_len(ch.len_utf8()),
                });

                pos.advance_by(ch.len_utf8());
            }
            _ if num_underscores == 1 => {
                errors.push(Error::MisplacedUnderscoreInDigits {
                    span: pos.with_backwards_len(num_underscores),
                });
                break;
            }
            _ => break,
        }

        num_underscores = UNDERSCORES.find(&code[pos.idx()..]).unwrap().len();
        pos.advance_by(num_underscores);
        is_pure &= num_underscores == 0;
        if 1 < num_underscores {
            errors.push(Error::MisplacedUnderscoreInDigits {
                span: pos.with_backwards_len(num_underscores),
            });
        }
    }

    if digits.is_empty() && !allow_no_digits {
        errors.push(Error::ExpectedDigits {
            span: start_pos.to(pos).unwrap(),
            base: base as _,
        });
    }

    (pos, digits, is_pure)
}

pub fn lex_number(
    code: &str,
    mut pos: Pos,
    tokens: &mut Vec<Token>,
    errors: &mut Vec<Error>,
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
        pos.advance_by(2);
        base
    };

    let (whole_digits, tmp_is_pure);
    (pos, whole_digits, tmp_is_pure) = parse_digits(code, pos, base, false, errors);
    is_pure &= tmp_is_pure;

    let frac_digits = if code[pos.idx()..].starts_with('.') {
        is_pure = false;
        pos.advance_by(1);

        let frac_digits;
        (pos, frac_digits, _) = parse_digits(code, pos, base, true, errors);
        Some(frac_digits)
    } else {
        None
    };

    let power_digits = match code[pos.idx()..].chars().next() {
        Some(ch @ ('e' | 'p')) => {
            is_pure = false;
            pos.advance_by(1);

            match (ch, base) {
                ('e', 10) | ('p', 16) => {}
                ('p', 10) => {
                    errors.push(Error::DecimalFloatHasHexPower {
                        span: pos.with_backwards_len(1),
                    });
                }
                ('e', 16) => unreachable!(),
                (_, _) => {
                    // ignore
                }
            }

            let neg = match code[pos.idx()..].chars().next() {
                Some(ch @ ('+' | '-')) => {
                    pos.advance_by(1);
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

    let suffix = SUFFIX.captures(&code[pos.idx()..]).map(|captures| {
        is_pure = false;

        let suffix = captures.get(2).unwrap().as_str();

        if captures.get(1).is_some() {
            pos.advance_by(1);
        }
        let suffix_start = pos;
        pos.advance_by(suffix.len());

        let suffix_span = suffix_start.to(pos).unwrap();

        if captures.get(1).is_none() {
            errors.push(Error::UnmarkedNumberSuffix { span: suffix_span })
        }

        let num_suffix = match suffix {
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
        };
        (num_suffix, suffix_span)
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
                errors.push(Error::InvalidFloatSuffix { span });
                FloatType::Auto
            }
            None => FloatType::Auto,
        };

        match base {
            10 => {
                let mut bytes = SmallVec::<[u8; 128]>::new();
                bytes.extend(whole_digits.iter().map(|&d| b'0' | d));
                bytes.push(b'.');
                bytes.extend(frac_digits.iter().map(|&d| b'0' | d));
                bytes.push(b'e');
                if is_power_neg {
                    bytes.push(b'-');
                }
                bytes.extend(power_digits.iter().map(|&d| b'0' | d));

                let value = unsafe { str::from_utf8_unchecked(&bytes) }
                    .parse::<f64>()
                    .unwrap();

                tokens.push(Token {
                    value: TokenValue::Float(value, ty),
                    span: start_pos.to(pos).unwrap(),
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
                            errors.push(Error::AccidentalZeroFloat {
                                span: start_pos.to(pos).unwrap(),
                            });
                            0.0
                        }
                        (num::bigint::Sign::Plus, _) => {
                            errors.push(Error::OverflowingNumberLiteral {
                                span: start_pos.to(pos).unwrap(),
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
                    span: start_pos.to(pos).unwrap(),
                });
            }
            _ => {
                errors.push(Error::InvalidFloatBase {
                    span: start_pos.to(pos).unwrap(),
                    base: base as _,
                });

                tokens.push(Token {
                    value: TokenValue::Float(1.0, ty),
                    span: start_pos.to(pos).unwrap(),
                });
            }
        }
    } else {
        let ty = match suffix {
            Some((NumberSuffix::Int(ty), _)) => ty,
            Some((NumberSuffix::Float(_), _)) => unreachable!(),
            Some((NumberSuffix::Unknown, span)) => {
                errors.push(Error::InvalidNumberSuffix { span });
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
            span: start_pos.to(pos).unwrap(),
        });
    }

    pos
}

pub fn parse_char_escape(code: &str, mut pos: Pos, errors: &mut Vec<Error>) -> (Pos, Option<char>) {
    let start_pos = pos;

    match code[pos.idx()..].chars().next() {
        Some('\\') => {
            pos.advance_by(1);

            match code[pos.idx()..].chars().next() {
                Some(ch @ ('a' | 'b' | 'f' | 'n' | 'r' | 't' | 'v' | '0' | '\\' | '\'' | '"')) => {
                    pos.advance_by(1);
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
                    pos.advance_by(1);
                    let Some(dg1) = code[pos.idx()..]
                        .chars()
                        .next()
                        .and_then(|ch| ch.to_digit(16))
                    else {
                        errors.push(Error::AsciiCharEscapeTooShort {
                            span: start_pos.to(pos).unwrap(),
                        });
                        return (pos, Some('\0'));
                    };
                    pos.advance_by(1);

                    let Some(dg2) = code[pos.idx()..]
                        .chars()
                        .next()
                        .and_then(|ch| ch.to_digit(16))
                    else {
                        errors.push(Error::AsciiCharEscapeTooShort {
                            span: start_pos.to(pos).unwrap(),
                        });
                        return (pos, Some(char::from_u32(dg1).unwrap()));
                    };
                    pos.advance_by(1);

                    if 7 < dg2 {
                        errors.push(Error::OutOfRangeAsciiEscape {
                            span: pos.with_backwards_len(2),
                        });
                    }

                    (pos, Some(char::from_u32(16 * dg1 + dg2).unwrap()))
                }
                Some('u') => {
                    pos.advance_by(1);

                    if !code[pos.idx()..].starts_with('{') {
                        errors.push(Error::IncorrectUnicodeEscapeSequence {
                            span: start_pos.to(pos).unwrap(),
                        });
                        return (pos, Some('\0'));
                    }
                    pos.advance_by(1);

                    let digits;
                    (pos, digits, _) = parse_digits(code, pos, 16, false, errors);

                    match code[pos.idx()..].chars().next() {
                        Some('}') => {}
                        Some(ch) => {
                            errors.push(Error::InvalidCharacterInUnicodeEscape {
                                span: pos.with_len(ch.len_utf8()),
                                ch,
                            });
                        }
                        None => {
                            errors.push(Error::UnterminatedUnicodeEscape {
                                span: start_pos.to(pos).unwrap(),
                            });
                        }
                    }

                    let v = digits
                        .iter()
                        .rev()
                        .enumerate()
                        .try_fold(0, |v, (i, &d)| {
                            Some(v | (d as u32).checked_shl(4 * i as u32)?)
                        })
                        .unwrap_or_else(|| {
                            errors.push(Error::OverlongUnicodeEscape {
                                span: start_pos.to(pos).unwrap(),
                            });
                            0
                        });

                    let ch = char::from_u32(v).unwrap_or_else(|| {
                        errors.push(Error::InvalidUnicodeCharacterEscape {
                            span: start_pos.to(pos).unwrap(),
                            escape_val: v,
                        });
                        '\0'
                    });
                    (pos, Some(ch))
                }
                Some(ch) => {
                    pos.advance_by(ch.len_utf8());
                    errors.push(Error::UnknownCharEscape {
                        escape: ch,
                        span: start_pos.to(pos).unwrap(),
                    });
                    (pos, Some(ch))
                }
                None => (pos, None),
            }
        }
        Some(ch) => {
            pos.advance_by(ch.len_utf8());
            (pos, Some(ch))
        }
        None => (pos, None),
    }
}

/// Must start with the regex: `^'`
pub fn lex_char(code: &str, mut pos: Pos, tokens: &mut Vec<Token>, errors: &mut Vec<Error>) -> Pos {
    let start_pos = pos;
    pos.advance_by(1);

    let ch;
    (pos, ch) = parse_char_escape(code, pos, errors);

    match ch {
        Some('\'') => {
            errors.push(Error::EmptyCharLiteral {
                span: start_pos.to(pos).unwrap(),
            });
            tokens.push(Token {
                value: TokenValue::Char('\0'),
                span: start_pos.to(pos).unwrap(),
            });
        }
        Some(ch) if code[pos.idx()..].starts_with('\'') => {
            pos.advance_by(1);
            tokens.push(Token {
                value: TokenValue::Char(ch),
                span: start_pos.to(pos).unwrap(),
            });
        }
        Some(ch) => {
            tokens.push(Token {
                value: TokenValue::Char(ch),
                span: start_pos.to(pos).unwrap(),
            });
            errors.push(Error::UnclosedCharLiteral {
                span: start_pos.to(pos).unwrap(),
            });
        }
        None => {
            tokens.push(Token {
                value: TokenValue::Char('\0'),
                span: start_pos.to(pos).unwrap(),
            });
            errors.push(Error::UnclosedCharLiteral {
                span: start_pos.to(pos).unwrap(),
            });
            todo!();
        }
    }

    pos
}

/// Must start with the regex: `^(?:[\w&&\D]\w*)?"`
pub fn lex_str(code: &str, mut pos: Pos, tokens: &mut Vec<Token>, errors: &mut Vec<Error>) -> Pos {
    lazy_static! {
        static ref STR_START_DELIM: Regex = Regex::new(r#"^"+"#).unwrap();
        static ref FORMAT_SPEC: Regex = Regex::new(
            r#"(?x)
                ^
                (?:
                    (?P<fill> \\\$ | \\\{ | \\\\ | [^\{\$\\] )? # }}
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
        static ref PROC_POS: Regex = Regex::new(r#"(?x) \{ | \\ | "+ "#).unwrap(); // }
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
                errors.push(Error::UnknownStrFlag {
                    span: pos.with_len(ch.len_utf8()),
                    flag: ch,
                });
                None
            }
        };
        match flag_span {
            Some(&mut Some(span1)) => errors.push(Error::DuplicateStrFlag {
                span1,
                span2: pos.with_len(1),
                flag: ch,
            }),
            Some(flag_span @ None) => *flag_span = Some(pos.with_len(1)),
            None => {}
        }
        pos.advance_by(ch.len_utf8());
    }

    let delim_len = STR_START_DELIM
        .find(&code[pos.idx()..])
        .unwrap()
        .as_str()
        .len();
    pos.advance_by(delim_len);

    if delim_len == 2 {
        tokens.push(Token {
            value: match format_flag_span {
                Some(_) => TokenValue::FStr(String::new(), vec![]),
                None => TokenValue::Str(String::new()),
            },
            span: start_pos.to(pos).unwrap(),
        });
    } else {
        let mut result = String::new();
        let mut fmt_args = vec![];

        if 2 < delim_len {
            if code[pos.idx()..].starts_with(r#"\""#) {
                pos.advance_by(2);
                result.push('"');
            } else if code[pos.idx()..].starts_with(r#"\\""#) {
                pos.advance_by(2);
                result.push('\\');
            }
        }

        loop {
            if let Some(mch) = PROC_POS.find(&code[pos.idx()..]) {
                let s = &code[pos.idx()..pos.idx() + mch.start()];
                pos.advance_by(mch.start());
                result.push_str(s);

                let seg_start_pos = pos;

                match mch.as_str() {
                    "{" => {
                        // }
                        pos.advance_by(1);
                        if format_flag_span.is_some() {
                            if code[pos.idx()..].starts_with("{" /* } */) {
                                pos.advance_by(1);
                                result.push('{'); // }
                            } else {
                                let mut tk_tokens = vec![];
                                pos = lex_group(
                                    code,
                                    pos,
                                    &mut tk_tokens,
                                    errors,
                                    Delim::Brace,
                                    pos.with_backwards_len(1),
                                );
                                fmt_args.push((
                                    result.len(),
                                    Token {
                                        value: TokenValue::Group(Delim::Brace, tk_tokens),
                                        span: seg_start_pos.to(pos).unwrap(),
                                    },
                                ));
                            }
                        } else {
                            result.push('{'); // }
                        }
                    }
                    "\\" => {
                        if raw_flag_span.is_some() {
                            pos.advance_by(1);
                            result.push('\\');
                        } else {
                            let ch;
                            (pos, ch) = parse_char_escape(code, pos, errors);
                            result.push(ch.unwrap());
                        }
                    }
                    closer => {
                        pos.advance_by(closer.len());
                        if closer.len() < delim_len {
                            result.push_str(closer);
                        } else {
                            result.push_str(&closer[delim_len..]);
                            break;
                        }
                    }
                }
            } else {
                pos.go_to(code.len());
                // Unclosed string
                errors.push(Error::UnclosedStr {
                    span: start_pos.to(pos).unwrap(),
                });
                break;
            }
        }

        tokens.push(Token {
            value: match (format_flag_span, multiline_flag_span) {
                (Some(_), Some(_)) => todo!(),
                (Some(_), None) => TokenValue::FStr(result, fmt_args),
                (None, Some(_)) => {
                    let mut out = String::new();
                    unindent(&mut out, &result);
                    TokenValue::Str(out)
                }
                (None, None) => TokenValue::Str(result),
            },
            span: start_pos.to(pos).unwrap(),
        });
    }

    pos
}

pub fn lex_group(
    code: &str,
    mut pos: Pos,
    tokens: &mut Vec<Token>,
    errors: &mut Vec<Error>,
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
            static ref START_TY: Regex = Regex::new(&format!(
                r#"(?x)
                    ^(?P<num>\d)
                      | (?P<group>[\(\[\{{\}}\]\)])
                      | (?P<str>(?:[\w&&\D]\w*)?")
                      | (?P<char_w>'[\w&&\D]')
                      | (?P<label>'[\w&&\D]\w*)
                      | (?P<char>')
                      | (?P<ident>(?:r')?[\w&&\D]\w*)
                      | (?P<punct>[{puncts}]+)
                      | (?P<any>.)
                "#,
                puncts = regex::escape(PUNCT_CHARS),
            ))
            .unwrap();
        }

        let ty = START_TY.captures(&code[pos.idx()..]).unwrap();

        if ty.name("num").is_some() {
            pos = lex_number(code, pos, tokens, errors);
        } else if let Some(mch) = ty.name("group") {
            let start_pos = pos;
            pos.advance_by(1);
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
                        pos.with_backwards_len(1),
                    );
                    tokens.push(Token {
                        value: TokenValue::Group(tk_delim, tk_tokens),
                        span: start_pos.to(pos).unwrap(),
                    });
                }
                ")" | "]" | "}" => {
                    if tk_delim != opening_delim {
                        errors.push(Error::MismatchedDelimeters {
                            open_delim: opening_delim,
                            close_delim: tk_delim,

                            open_span: opening_delim_span,
                            close_span: pos.with_backwards_len(1),
                        });
                    }
                    return pos;
                }
                _ => unreachable!(),
            }
        } else if ty.name("str").is_some() {
            pos = lex_str(code, pos, tokens, errors);
        } else if ty.name("char").is_some() || ty.name("char_w").is_some() {
            pos = lex_char(code, pos, tokens, errors);
        } else if let Some(mch) = ty.name("label") {
            tokens.push(Token {
                value: TokenValue::Label(mch.as_str().trim_start_matches('\'').to_owned()),
                span: pos.with_len(mch.len()),
            });

            pos.advance_by(mch.len());
        } else if let Some(mch) = ty.name("ident") {
            let s = mch.as_str();
            tokens.push(Token {
                value: TokenValue::Ident {
                    raw: s.starts_with("r'"),
                    ident: s.trim_start_matches("r'").to_owned(),
                },
                span: pos.with_len(mch.len()),
            });

            pos.advance_by(mch.len());
        } else if let Some(mch) = ty.name("punct") {
            for &ch in mch.as_str().as_bytes() {
                tokens.push(Token {
                    value: TokenValue::Punct(Punct::new(ch)),
                    span: pos.with_len(1),
                });

                pos.advance_by(1);
            }
            tokens.push(Token {
                value: TokenValue::Punct(Punct::end()),
                span: pos.with_len(0),
            });
        } else if let Some(mch) = ty.name("any") {
            let ch = mch.as_str().chars().next().unwrap();
            errors.push(Error::IllegalCharacter {
                ch,
                span: pos.with_len(ch.len_utf8()),
            });

            pos.advance_by(ch.len_utf8());
        } else {
            unreachable!();
        }
    }

    if opening_delim != Delim::None {
        errors.push(Error::UnclosedDelimeter {
            delim: opening_delim,
            open_span: opening_delim_span,
            span: opening_delim_span.merged_with_pos(&pos).unwrap(),
        });
    }

    pos
}

pub fn lex(buf: u32, code: &str) -> (Vec<Token>, Vec<Error>) {
    let mut pos = Pos::zero(buf);
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
