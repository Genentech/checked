#' Create a 'cli' Spinner With Suppressed Output
#'
#' 'cli' will implicitly push spinner output to various output streams,
#' affecting the terminal cursor position. To allow for a terminal interface
#' that has spinners above the last line, this function suppresses the output
#' and simply returns its frame contents.
#'
#' @param ... passed to [cli::make_spinner]
#' @param stream passed to [cli::make_spinner], defaults to a null file device
#' @return A interface similar to a 'cli' spinner, but with suppressed output
#'
#' @importFrom cli make_spinner
#' @keywords internal
silent_spinner <- function(..., stream = devnull()) {
  spinner <- cli::make_spinner(..., stream = stream)

  spin <- function(...) {
    spinner$spin(...)
    with(environment(spinner$spin), c_spinner$frames[[c_state]])
  }

  list(spin = spin, final = spinner$final)
}

#' Reuse or Create A Null File Connection
#' @keywords internal
devnull <- function() {
  cons <- showConnections(all = TRUE)[, "description"]
  if (length(w <- which(nullfile() == cons))) {
    getConnection(names(cons)[[w[[1]]]])
  } else {
    file(nullfile())
  }
}

format_time <- function(x) {
  n <- unclass(x)
  sprintf("%.01f%s", n, substring(attr(n, "units"), 1, 1))
}

str_pad <- function(x, n) {
  x <- format(x)
  paste0(strrep(" ", n - nchar(x)), x)
}

emoji <- list(
  dev = "\U0001F6A7",
  release = "\U0001F680"
)
