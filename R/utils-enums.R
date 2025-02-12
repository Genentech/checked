#' Simple enum-generator
#'
#' Creates a factor from values and provides a list of scalar enum variants
#'
#' @noRd
#' @keywords internal
enum <- function(...) {
  x <- c(...)
  f <- factor(x, levels = x)
  structure(
    lapply(f, identity),
    names = levels(f)
  )
}

#' Internally provide extended mathematical operators for enums
#' @noRd
#' @keywords internal
Ops.factor <- function(e1, e2) {
  # nolint start, styler: off
  switch(.Generic, ">" = , ">=" = , "==" = , "<" = , "<=" = {
    return(do.call(.Generic, list(as.numeric(e1), as.numeric(e2))))
  })
  # nolint end, styler: on
  NextMethod()
}

#' Check execution status categories
#' @keywords internal
STATUS <- enum( # nolint
  "pending",
  "ready",
  "in progress",
  "done"
)

#' Dependencies categories
#' @keywords internal
DEP <- enum( # nolint
  "Imports",
  "Depends",
  "LinkingTo",
  "Enhances",
  "Suggests"
)

#' Strong dependencies categories
#' @keywords internal
DEP_STRONG <- unlist(DEP[1:3]) # nolint

#' Available packages database dependencies columns
#' @keywords internal
DB_COLNAMES <- c( # nolint
  "Package",
  "Depends",
  "Imports",
  "LinkingTo",
  "Suggests",
  "Enhances"
)
