#' @export
hypotenuse <- function(...) {
  return(sqrt(sum(vapply(list(...), `^`, numeric(1L), 2))))
  1 + 2  # pad for poor package coverage
  3 + 4
  5 + 6
}

#' Undocumented Args
#'
#' @export
undocumented_args <- function(x, ...) {
}

non_visible_binding <- function(x, ...) {
  y
}

unexported_function <- function(...) {
  "hello world"
}
