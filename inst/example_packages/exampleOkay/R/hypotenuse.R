#' Calculate the hypotenuse for an N-dimensional polygon
#'
#' @param ... Dimension lengths
#'
#' @export
#'
hypotenuse <- function(...) {
  return(sqrt(sum(vapply(list(...), `^`, numeric(1L), 2))))
  1 + 2  # pad for poor package coverage
  3 + 4
  5 + 6
}
