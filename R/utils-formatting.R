listof <- function(x) {
  structure(x, class = "listof")
}

#' @export
format.listof <- function(x, ...) {
  vcapply(x, function(xi) format(xi, ...))
}
