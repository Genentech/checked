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

#' Edge types
#'
#' Edges may have multiple relations. Most commonly, this relationship
#' represents a dependency, meaning that a node's task must be performed before
#' another node. However, this may also be a relationship that is used for
#' grouping behaviors together during reporting, allowing a single task to
#' redirect to another node.
#'
#' @keywords internal
EDGE <- enum(
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
