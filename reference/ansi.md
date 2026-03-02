# Various utilities for formatting ANSI output

Various utilities for formatting ANSI output

## Usage

``` r
ansi_line_erase(n = "")

ansi_move_line_rel(n)

ansi_tty_height()
```

## Arguments

- n:

  The number of lines to move. Positive is up, negative is down.

## Functions

- `ansi_line_erase()`: Erase the current line

- `ansi_move_line_rel()`: Offset the cursor by a relative number of
  lines

- `ansi_tty_height()`: Get the height of the ansi tty using 'tput lines'
  interface
