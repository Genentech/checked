#' Various utilities for formatting ANSI output
#' @name ansi
NULL

#' @describeIn ansi
#' Erase the current line
#'
#' @param n The number of characters to erase, defaults to an empty string,
#'   which will erase the entire line.
ansi_line_erase <- function(n = "") {
  paste0("\033[", n, "K")
}

#' @describeIn ansi
#' Offset the cursor by a relative number of lines
#'
#' @param n The number of lines to move. Positive is up, negative is down.
ansi_move_line_rel <- function(n) {
  paste0("\033[", abs(n), if (n > 0L) "F" else "E")
}
