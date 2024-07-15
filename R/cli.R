cli_table_row <- function(
    status, ok = "OK", notes = "N", warnings = "W", errors = "E", msg = "",
    title = FALSE) {
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
    if (title) cli::col_none(cli::style_bold(status)) else status
  )

  ok <- str_pad(ok, n = 2)
  notes <- str_pad(notes, n = 2)
  warnings <- str_pad(warnings, n = 2)
  errors <- str_pad(errors, n = 2)

  if (title) {
    ok <- cli::col_none(cli::style_bold(ok))
    notes <- cli::col_none(cli::style_bold(notes))
    warnings <- cli::col_none(cli::style_bold(warnings))
    errors <- cli::col_none(cli::style_bold(errors))
  } else {
    ok <- cli::format_inline("{.ok {ok}}")
    notes <- cli::format_inline("{.note {notes}}")
    warnings <- cli::format_inline("{.warn {warnings}}")
    errors <- cli::format_inline("{.err {errors}}")
  }

  fmt <- "\u2502 {status} \u2502 {ok} {notes} {warnings} {errors} \u2502 {msg}"
  cli::format_inline(fmt)
}

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
