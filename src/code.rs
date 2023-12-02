use core::fmt;
use std::{io, ops, path::PathBuf};

use lazy_static::lazy_static;
use regex::Regex;

pub struct CodeBuf {
    path: PathBuf,
    code: String,
    code_lines: Vec<u32>,
}

impl CodeBuf {
    pub fn code(&self) -> &str {
        &self.code
    }
}

pub struct CodeBufs {
    bufs: Vec<CodeBuf>,
}

impl CodeBufs {
    pub fn new() -> Self {
        CodeBufs { bufs: vec![] }
    }

    pub fn add_file(&mut self, path: PathBuf, code: String) {
        lazy_static! {
            static ref NEWLINE: Regex = Regex::new(r"(?m)^").unwrap();
        }
        self.bufs.push(CodeBuf {
            code_lines: NEWLINE
                .find_iter(&code)
                .map(|mch| mch.start() as _)
                .collect(),
            code,
            path,
        });
    }

    #[inline]
    pub fn bufs(&self) -> &[CodeBuf] {
        &self.bufs
    }

    #[inline]
    pub fn buf(&self, i: usize) -> &CodeBuf {
        &self.bufs[i]
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(align(8))]
pub struct Pos {
    idx: u32,
    buf: u32,
}

impl Pos {
    #[inline]
    pub const fn new(buf: u32, idx: usize) -> Self {
        Self { buf, idx: idx as _ }
    }

    #[inline]
    pub const fn zero(buf: u32) -> Self {
        Self::new(buf, 0)
    }

    pub fn count_col(&self, code: &str) -> usize {
        let start = code[..self.idx()].rfind('\n').map_or(0, |i| i + 1);
        code[start..self.idx()].chars().count() + 1
    }

    #[inline]
    pub const fn idx(&self) -> usize {
        self.idx as _
    }

    #[inline]
    pub const fn buf(&self) -> u32 {
        self.buf
    }

    #[inline]
    pub fn go_to(&mut self, new_idx: usize) {
        self.idx = new_idx as _;
    }

    #[inline]
    pub fn advance_by(&mut self, len: usize) {
        self.idx += len as u32;
    }

    #[inline]
    pub fn retreat_by(&mut self, len: usize) {
        self.idx -= len as u32;
    }

    #[inline]
    pub const fn to(self, end: Pos) -> Option<Span> {
        if self.buf == end.buf {
            Some(Span {
                start: self.idx,
                end: end.idx,
                buf: self.buf,
            })
        } else {
            None
        }
    }

    #[inline]
    pub const fn with_len(self, len: usize) -> Span {
        Span {
            start: self.idx,
            end: self.idx + len as u32,
            buf: self.buf,
        }
    }

    #[inline]
    pub const fn with_backwards_len(self, len: usize) -> Span {
        Span {
            start: self.idx - len as u32,
            end: self.idx,
            buf: self.buf,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(align(8))]
pub struct Span {
    start: u32,
    end: u32,
    buf: u32,
}

impl Span {
    #[inline]
    pub const fn new(buf: u32, start: usize, end: usize) -> Self {
        Self {
            start: start as _,
            end: end as _,
            buf,
        }
    }

    #[inline]
    pub const fn zero(buf: u32) -> Self {
        Self::new(buf, 0, 0)
    }

    #[inline]
    pub const fn buf(&self) -> u32 {
        self.buf
    }

    #[inline]
    pub const fn start_pos(&self) -> Pos {
        Pos::new(self.buf, self.start_idx())
    }

    #[inline]
    pub const fn start_span(&self) -> Self {
        self.start_pos().with_len(0)
    }

    #[inline]
    pub const fn end_pos(&self) -> Pos {
        Pos::new(self.buf, self.end_idx())
    }

    #[inline]
    pub const fn end_span(&self) -> Self {
        self.end_pos().with_len(0)
    }

    #[inline]
    pub const fn start_idx(&self) -> usize {
        self.start as _
    }

    #[inline]
    pub const fn end_idx(&self) -> usize {
        self.end as _
    }

    #[inline]
    pub const fn len(&self) -> usize {
        (self.end - self.start) as _
    }

    #[inline]
    pub const fn idx_range(&self) -> ops::Range<usize> {
        self.start as _..self.end as _
    }

    #[inline]
    pub const fn merged_with(&self, other: &Self) -> Option<Self> {
        #[inline]
        const fn max(a: u32, b: u32) -> u32 {
            if a < b {
                b as _
            } else {
                a as _
            }
        }
        #[inline]
        const fn min(a: u32, b: u32) -> u32 {
            if a < b {
                a as _
            } else {
                b as _
            }
        }

        if self.buf == other.buf {
            Some(Self {
                start: min(self.start, other.start),
                end: max(self.end, other.end),
                buf: self.buf,
            })
        } else {
            None
        }
    }

    #[inline]
    pub const fn merged_with_pos(&self, pos: &Pos) -> Option<Self> {
        #[inline]
        const fn max(a: u32, b: u32) -> u32 {
            if a < b {
                b as _
            } else {
                a as _
            }
        }
        #[inline]
        const fn min(a: u32, b: u32) -> u32 {
            if a < b {
                a as _
            } else {
                b as _
            }
        }

        if self.buf == pos.buf {
            Some(Self {
                start: min(self.start, pos.idx),
                end: max(self.end, pos.idx),
                buf: self.buf,
            })
        } else {
            None
        }
    }

    pub fn show(self, bufs: &'_ CodeBufs, mut writer: impl io::Write) -> io::Result<()> {
        use crossterm::style::{
            Attribute, Color, Print, ResetColor, SetAttribute, SetForegroundColor,
        };

        const LINE_COLOR: Color = Color::Magenta;
        const SPANNED_COLOR: Color = Color::Red;

        let buf = &bufs.bufs[self.buf as usize];

        let last_line = (buf.code_lines).partition_point(|&line_start| line_start < self.end);
        let first_line = (buf.code_lines)
            .partition_point(|&line_start| line_start <= self.start)
            .min(last_line);

        let first_line_start = buf.code_lines[first_line - 1] as usize;
        let first_line_end = (buf.code_lines)
            .get(first_line)
            .map_or(buf.code.len(), |&start| start as _);

        let last_line_start = buf.code_lines[last_line - 1] as usize;
        let last_line_end = (buf.code_lines)
            .get(last_line)
            .map_or(buf.code.len(), |&start| start as _);

        let start_col = buf.code[first_line_start..self.start_idx()].chars().count() + 1;
        let end_col = buf.code[last_line_start..self.end_idx()].chars().count();

        writeln!(
            writer,
            "From {}:{first_line}:{start_col} till {last_line}:{end_col}",
            buf.path.display()
        )?;

        // let line_width = last_line.ilog10() as usize + 1;
        let line_width = buf.code_lines.len().ilog10() as usize + 1;

        if (buf.code_lines)
            .get(first_line + 1)
            .is_some_and(|&line_start| self.start + 1 == line_start)
            && self.len() == 1
        {
            // newline

            crossterm::queue!(
                writer,
                SetForegroundColor(LINE_COLOR),
                Print(format_args!("{first_line: >line_width$} | ")),
                ResetColor,
                Print(
                    &buf.code[first_line_start..last_line_end]
                        .trim_end_matches('\n')
                        .trim_end_matches('\r')
                ),
                SetForegroundColor(SPANNED_COLOR),
                SetAttribute(Attribute::Underlined),
                Print('⏎'), // ⮒
                SetAttribute(Attribute::NoUnderline),
                ResetColor,
                Print('\n'),
            )?;
        } else if self.start_idx() == buf.code.len() {
            let line_start = buf.code_lines[first_line - 2] as usize;
            crossterm::queue!(
                writer,
                SetForegroundColor(LINE_COLOR),
                Print(format_args!("{: >line_width$} | ", first_line - 1)),
                ResetColor,
                Print(&buf.code[line_start..first_line_start]),
                SetForegroundColor(LINE_COLOR),
                Print(format_args!("{first_line: >line_width$} | ")),
                ResetColor,
                if first_line_start < buf.code.len() {
                    Print(&buf.code[first_line_start..])
                } else {
                    Print("\n")
                },
                Print(format_args!(
                    "{: >lw$}\\\n",
                    '/',
                    lw = line_width + 2 + self.start_idx() - first_line_start,
                )),
            )?;
        } else if self.start == self.end {
            crossterm::queue!(
                writer,
                SetForegroundColor(LINE_COLOR),
                Print(format_args!("{first_line: >line_width$} | ")),
                ResetColor,
                Print(&buf.code[first_line_start..last_line_end].trim_end()),
                SetForegroundColor(SPANNED_COLOR),
                Print(format_args!(
                    "\n{: >1$}\\\n",
                    '/',
                    line_width + 3 + self.start_idx() - first_line_start,
                )),
                ResetColor,
            )?;
        } else if first_line == last_line {
            crossterm::queue!(
                writer,
                SetForegroundColor(LINE_COLOR),
                Print(format_args!("{first_line: >line_width$} | ")),
                ResetColor,
                Print(&buf.code[first_line_start..self.start_idx()]),
                SetForegroundColor(SPANNED_COLOR),
                SetAttribute(Attribute::Underlined),
                Print(&buf.code[self.idx_range()]),
                SetAttribute(Attribute::NoUnderline),
                ResetColor,
                Print(&buf.code[self.end_idx()..last_line_end]),
            )?;
        } else if last_line <= first_line + 7 {
            crossterm::queue!(
                writer,
                SetForegroundColor(LINE_COLOR),
                Print(format_args!("{first_line: >line_width$} | ")),
                ResetColor,
                Print(&buf.code[first_line_start..self.start_idx()]),
                SetForegroundColor(SPANNED_COLOR),
                SetAttribute(Attribute::Underlined),
                Print(&buf.code[self.start_idx()..first_line_end]),
                SetAttribute(Attribute::NoUnderline),
                ResetColor,
            )?;
            for (line, window) in
                (first_line + 1..).zip(buf.code_lines[first_line..last_line].windows(2))
            {
                let &[line_start, line_end] = window else {
                    unreachable!();
                };

                crossterm::queue!(
                    writer,
                    SetForegroundColor(LINE_COLOR),
                    Print(format_args!("{line: >line_width$} | ")),
                    SetForegroundColor(SPANNED_COLOR),
                    SetAttribute(Attribute::Underlined),
                    Print(&buf.code[line_start as usize..line_end as usize]),
                    SetAttribute(Attribute::NoUnderline),
                    ResetColor,
                )?;
            }
            crossterm::queue!(
                writer,
                SetForegroundColor(LINE_COLOR),
                Print(format_args!("{last_line: >line_width$} | ")),
                ResetColor,
                SetForegroundColor(SPANNED_COLOR),
                SetAttribute(Attribute::Underlined),
                Print(&buf.code[last_line_start..self.end_idx()]),
                SetAttribute(Attribute::NoUnderline),
                ResetColor,
                Print(&buf.code[self.end_idx()..last_line_end]),
            )?;
        } else {
            crossterm::queue!(
                writer,
                SetForegroundColor(LINE_COLOR),
                Print(format_args!("{first_line: >line_width$} | ")),
                ResetColor,
                Print(&buf.code[first_line_start..self.start_idx()]),
                SetForegroundColor(SPANNED_COLOR),
                SetAttribute(Attribute::Underlined),
                Print(&buf.code[self.start_idx()..first_line_end]),
                SetAttribute(Attribute::NoUnderline),
                ResetColor,
            )?;
            for (line, window) in
                (first_line + 1..).zip(buf.code_lines[first_line..first_line + 3].windows(2))
            {
                let &[line_start, line_end] = window else {
                    unreachable!();
                };

                crossterm::queue!(
                    writer,
                    SetForegroundColor(LINE_COLOR),
                    Print(format_args!("{line: >line_width$} | ")),
                    SetForegroundColor(SPANNED_COLOR),
                    SetAttribute(Attribute::Underlined),
                    Print(&buf.code[line_start as usize..line_end as usize]),
                    SetAttribute(Attribute::Reset),
                    ResetColor,
                )?;
            }
            crossterm::queue!(
                writer,
                SetForegroundColor(LINE_COLOR),
                Print(format_args!("{: >lw$} | ", ' ', lw = line_width)),
                ResetColor,
                SetAttribute(Attribute::Dim),
                Print("...\n"),
                SetAttribute(Attribute::Reset),
            )?;
            for (line, window) in
                (last_line - 2..).zip(buf.code_lines[last_line - 3..last_line].windows(2))
            {
                let &[line_start, line_end] = window else {
                    unreachable!();
                };

                crossterm::queue!(
                    writer,
                    SetForegroundColor(LINE_COLOR),
                    Print(format_args!("{line: >line_width$} | ")),
                    SetForegroundColor(SPANNED_COLOR),
                    SetAttribute(Attribute::Underlined),
                    Print(&buf.code[line_start as usize..line_end as usize]),
                    SetAttribute(Attribute::Reset),
                    ResetColor,
                )?;
            }
            crossterm::queue!(
                writer,
                SetForegroundColor(LINE_COLOR),
                Print(format_args!("{last_line: >line_width$} | ")),
                ResetColor,
                SetForegroundColor(SPANNED_COLOR),
                SetAttribute(Attribute::Underlined),
                Print(&buf.code[last_line_start..self.end_idx()]),
                SetAttribute(Attribute::NoUnderline),
                ResetColor,
                Print(&buf.code[self.end_idx()..last_line_end]),
            )?;
        }

        Ok(())

        // fn find_line_start(code: &str, pos: &Pos) -> usize {
        //     if pos.line() == 1 {
        //         0
        //     } else if pos.column() == 1 {
        //         pos.idx()
        //     } else {
        //         code[..pos.idx()].rfind('\n').map_or(0, |i| i + 1)
        //     }
        // }

        // fn find_line_end(code: &str, pos: &Pos) -> usize {
        //     code[pos.idx()..].find('\n').map_or(code.len(), |i| i + 1)
        // }

        // let path = path.as_ref();

        // let mut end = self.start();
        // end.advance_str(&code[self.range()]);
        // let off = end.line().ilog10() as usize + 1;

        // println!("{:off$}--> {path:?}:{}:{}", "", self.line(), self.column());
        // match end.line() - self.line() {
        //     0 => println!(
        //         "{} | {}",
        //         self.line(),
        //         &code[find_line_start(code, &self.start())..find_line_end(code, &end)],
        //     ),
        //     1..=4 => todo!(),
        //     5.. => todo!(),
        // }
    }
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self { start, end, buf } = *self;
        write!(f, "Span({buf}, {start}..{end})")
    }
}
