# Internal Utilities for Command-line Output

Various helper functions for consistent cli output, including theming
and formatting.

## Usage

``` r
cli_table_row(
  status = "",
  ok = "OK",
  notes = "N",
  warnings = "W",
  errors = "E",
  msg = "",
  style = c("row", "title", "header"),
  symbols = DEFAULT_ROW_SYMBOL
)

cli_theme(..., .envir = parent.frame())
```

## Arguments

- status, ok, notes, warnings, errors:

  `character[1L]` A value to include in the respective columns of the
  table. Will be coerced to `character` if another type is provided.

- msg:

  `character[1L]` A message to include to the right of the table row
  entry.

- ..., .envir:

  Additional arguments passed to
  [`cli::cli_div()`](https://cli.r-lib.org/reference/cli_div.html)
