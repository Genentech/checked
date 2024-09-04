#' Task specification
#'
#' Create task specification list which consists of all the details required
#' to run specific task.
#'
#' @param alias task alias which also serves as unique identifier of the task.
#' @param package \code{\link[checked]{package}} object
#' @param env environmental variables to be set in separate process running
#' specific task.
#'
#' @family tasks
#' @export
task <- function(alias = NULL, package = NULL, env = NULL) {
  structure(list(alias = alias, package = package, env = env), class = "task")
}

list_of_task <- function(x, ...) {
  structure(x, class = c("list_of_task", "list"))
}

#' @family tasks
#' @export
print.task <- function(x, ...) {
  cat(format(x, ...), "\n")
}

#' @family tasks
#' @export
format.task <- function(x, ...) {
  paste0("<task ", x$alias, ">")
}

#' @family tasks
#' @export
format.list_of_task <- function(x, ...) {
  vcapply(x, format)
}

#' Create a task to install a package and dependencies
#'
#' @param ... Additional parameters passed to [`task()`]
#' @inheritParams utils::install.packages
#'
#' @family tasks
#' @export
install_task <- function(
  type = getOption("pkgType"),
  INSTALL_opts = NULL, # nolint: object_name_linter.
  ...
) {
  task <- task(...)
  task$type <- type
  task$INSTALL_opts <- INSTALL_opts
  class(task) <- c("install_task", class(task))
  task
}

#' Create a custom install task
#'
#' @inheritDotParams install_task
#'
#' @family tasks
#' @export
custom_install_task <- function(...) {
  task <- install_task(...)
  class(task) <- c("custom_install_task", class(task))
  task
}

#' Create a task to run `R CMD check`
#'
#' @inheritParams rcmdcheck::rcmdcheck
#' @inheritDotParams task
#'
#' @family tasks
#' @export
check_task <- function(args = NULL, build_args = NULL, ...) {
  task <- task(...)
  task$args <- args
  task$build_args <- build_args
  class(task) <- c("check_task", class(task))
  task
}

#' Create a task to run reverse dependency checks
#'
#' @param revdep character indicating whether the task specification describes
#' check associated with the development (new) or release (old) version of the
#' for which reverse dependency check is run.
#' @param ... Additional parameters passed to [`task()`]
#'
#' @family tasks
#' @export
revdep_check_task <- function(revdep, ...) {
  task <- check_task(...)
  task["revdep"] <- list(revdep)
  class(task) <- c("revdep_check_task", class(task))
  task
}
