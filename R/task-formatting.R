#' @export
format.task <- function(x, ..., indent = 0L) {
  fmt(task = x, "{action} {package} {version} {source}")
}

#' @export
format_task_name.task <- function(x, ...) {
  "task"
}

format_task_name <- function(x, ...) {
  UseMethod("format_task_name")
}

#' @export
format_task_name.default <- function(x, ...) {
  stop(
    "Dont' know how to name object with class(es) `",
    deparse(class(x)),
    "`"
  )
}

format_task_type <- function(x, ...) {
  UseMethod("format_task_type")
}

#' @export
format_task_type.task <- function(x, ...) {
  sub("_task$", "", class(x)[[1]])
}

#' @export
format_task_name.install_task <- function(x, ..., short = FALSE) {
  paste0(if (!short) "install ", format(x$origin, ..., short = short))
}

#' @export
format_task_name.check_task <- function(x, ...) {
  paste0("check ", format(x$origin, ...))
}

#' @export
format_task_name.rev_dep_dep_meta_task <- function(x, ..., short = FALSE) {
  paste0(
    "check ",
    format(x$origin, ..., short = short),
    if (short) " revdeps" else " reverse-dependencies"
  )
}

#' @export
format_task_name.rev_dep_check_meta_task <- function(x, ..., short = FALSE) {
  paste0("check ", if (short) "revdep" else "reverse-dependency")
}
