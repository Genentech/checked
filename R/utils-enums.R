#' Simple enum-generator
#'
#' Creates a factor from values and provides a list of scalar enum variants
#'
#' @noRd
#' @keywords internal
enum <- function(...) {
  x <- c(...)
  f <- factor(x, levels = x)
  names(f) <- as.character(f)
  structure(f, class = c("enum", "factor"))
}

#' Internally provide extended mathematical operators for enums
#' @noRd
#' @export
#' @keywords internal
Ops.enum <- function(e1, e2) {
  # nolint start, styler: off
  switch(.Generic, ">" = , ">=" = , "==" = , "<" = , "<=" = {
    return(do.call(.Generic, list(as.numeric(e1), as.numeric(e2))))
  })
  # nolint end, styler: on
  NextMethod()
}

#' Internally provide `$` for enum factors
#' @noRd
#' @keywords internal
#' @export
`$.enum` <- function(x, i) {
  x[[i]]
}

#' Relation types
#'
#' A flag for edges that articulate different relations between nodes.
#'
#' @keywords internal
RELATION <- enum(
  "dep",    # for task dependencies
  "report"  # for reporting graph, from task to reporting node
)

#' Check execution status categories
#' @keywords internal
STATUS <- enum(
  "pending",
  "in progress",
  "done"
)

#' Dependencies categories
#' @keywords internal
DEP <- enum(
  "Imports",
  "Depends",
  "LinkingTo",
  "Enhances",
  "Suggests"
)

#' Strong dependencies categories
#' @keywords internal
DEP_STRONG <- unlist(DEP[1:3])

#' Available packages database dependencies columns
#' @keywords internal
DB_COLNAMES <- c(
  "Package",
  "Depends",
  "Imports",
  "LinkingTo",
  "Suggests",
  "Enhances"
)
