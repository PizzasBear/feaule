# feaule

For now a fault-tolerant lexer,
that reports all errors while still outputing "placeholder" tokens.
In the future it might be a full programming language.

## Syntax

There are 12 different token types:

- A 64-bit float, [like in c](https://en.cppreference.com/w/c/language/floating_constant) (including hex-float).
  It is different in that it requires the whole part to be present,
  allows nonconsecutive separating underscores are allowed,
  and permits a suffix of either `'f32` or `'f64`.

  For example:

  ```
  0xffp+6'f32 = 255e+6'f32 = 255_000_000.0f32
  15. = 15.0 = 0d15e+0 = 0xfp+0 = 1_5.0_0
  ```

- A big integer, with a bit too many bases.
  Nonconsecutive separating underscores are allowed and encouraged,
  there are also a bunch of allowed suffixes:
  `'i8`, `'i16`, `'i32`, `'i64` `'iptr`, `'u8`, `'u16`, `'u32`, `'u64` `'uptr`.
  Pure integers are integers without underscores, specified bases or suffixes.

  | base | specifier characters       |
  | ---- | -------------------------- |
  | 2    | `0b..` or `0B..`           |
  | 3    | `0t..` or `0T..`           |
  | 4    | `0q..` or `0Q..`           |
  | 6    | `0s..` or `0S..`           |
  | 8    | `0o..` or `0O..`           |
  | 10   | `0d..` or `0D..` (default) |
  | 12   | `0z..` or `0Z..`           |
  | 16   | `0x..` or `0X..`           |

  For example:

  ```
  pure 12 = 0b1_100 = 0t110 = 0q30 = 0s20 = 0O14 = 0z10 = 0xC
  pure 65535 = 0xffff = 0xFfFf = 65_535 = 0D65535

  36'i32 = 0z30'i32
  ```

- A char is pretty much like rust.
- A string is complex and annoying to describe.
- A label is like it is in rust
- A punctuation is any one of these ``+-*/%,!&|^~$@<>=.,:;?'`\``.
  After the last punctuation in a sequence, `Punct::END` will be inserted.

<!--
- A 64-bit float, that matches the following pattern.
  ```
  digits "." (digits (i"e" ["+" "-"] digits)?)?
    | digits i"e" ["+" "-"] digits
    | i"0x" xdigits "." (xdigits (i"p" ["+" "-"] digits)?)?
    | i"0x" xdigits i"p" ["+" "-"] digits
  ```
- A big integer, that matches the following pattern.
  ```
  i"0x" xdigits,
    | i"0z" digits12,
    | i"0d" digits,
    | i"0o" digits8,
    | i"0s" digits6,
    | i"0q" digits4,
    | i"0t" digits3,
    | i"0b" bdigits,
  ```
-->
