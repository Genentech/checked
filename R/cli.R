#' Internal Utilities for Command-line Output
#'
#' Various helper functions for consistent cli output, including theming and
#' formatting.
#'
#' @param status,ok,notes,warnings,errors `character[1L]` A value to include
#'   in the respective columns of the table. Will be coerced to `character`
#'   if another type is provided.
#' @param msg `character[1L]` A message to include to the right of the  table
#'   row entry.
#' @param ...,.envir Additional arguments passed to [`cli::cli_div()`]
#'
#' @name cli
#' @keywords internal
NULL

#' @name cli
cli_table_row <- function(
  status = "",
  ok = "OK",
  notes = "N",
  warnings = "W",
  errors = "E",
  msg = "",
  style = c("row", "title", "header"),
  symbols = list(bar = "\u2502")
) {
  style <- match.arg(style)

  cli_theme()

  status <- trimws(as.character(status))
  status <- switch(status,
    "1" = ,
    "2" = ,
    "OK" = ,
    "3" = ,
    "NONE" = cli::format_inline("{.success \u2713}"),
    "4" = ,
    "NOTE" = cli::format_inline("{.note !}"),
    "5" = ,
    "WARNING" = cli::format_inline("{.warn ?}"),
    "6" = ,
    "ERROR" = cli::format_inline("{.err \u2a2f}"),
    switch(style,
      "title" = cli::col_none(cli::style_bold(status)),
      "header" = " ",
      status
    )
  )

  ok <- str_pad(ok, n = 2)
  notes <- str_pad(notes, n = 2)
  warnings <- str_pad(warnings, n = 2)
  errors <- str_pad(errors, n = 2)

  switch(
    style,
    "title" = {
      ok <- cli::col_none(cli::style_bold(ok))
      notes <- cli::col_none(cli::style_bold(notes))
      warnings <- cli::col_none(cli::style_bold(warnings))
      errors <- cli::col_none(cli::style_bold(errors))
    },
    "row" = {
      ok <- cli::format_inline("{.ok {ok}}")
      notes <- cli::format_inline("{.note {notes}}")
      warnings <- cli::format_inline("{.warn {warnings}}")
      errors <- cli::format_inline("{.err {errors}}")
    }
  )

  fmt <- "{symbols$bar} {status} {symbols$bar} {ok} {notes} {warnings} {errors} {symbols$bar} {msg}"  # nolint
  cli::format_inline(fmt)
}

#' @name cli
cli_theme <- function(..., .envir = parent.frame()) {
  cli_div(..., .envir = .envir, theme = list(
    span.ok = list(),
    span.time_active = list(color = "cyan"),
    span.time_taken = list(color = "grey"),
    span.success = list(color = "green"),
    span.err = list(color = "yellow"),
    span.warn = list(color = "magenta"),
    span.note = list(color = "blue")
  ))
}

cli_wrap_lines <- function(text, w = cli::console_width()) {
  n <- cli::ansi_nchar(text)
  cli::ansi_substring(text, seq_len(ceiling(n / w)) * w - w + 1, seq_len(ceiling(n / w)) * w)
}
