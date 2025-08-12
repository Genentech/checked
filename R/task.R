#' Task specification
#'
#' Create task specification list which consists of all the details required
#' to run specific task.
#'
#' Tasks can be nested, representing either a singular task, or a set of
#' related tasks.
#'
#' @param alias task alias which also serves as unique identifier of the task.
#' @param package \code{\link[checked]{package}} object
#' @param tasks Optional additional tasks that are pre-requisites for running
#'   this task.
#'
#' @family tasks
#' @export
task <- function(..., .subclass = NULL) {
  structure(list(...), class = c(sprintf("%s_task", .subclass), "task"))
}

#' Construct a 'Meta' Task
#'
#' Meta tasks are tasks which are not intended to perform computation. They
#' exist simply to provide relationships among computational tasks.
meta_task <- function(..., .subclass = NULL) {
  task(..., .subclass = c(sprintf("%s_meta", .subclass), "meta"))
}

make_unique_task <- function(task, seed = runif(1)) {
  task$seed <- seed
  task
}

#' @family tasks
#' @export
lib.task <- function(x, ...) {
  character(0L)
}

#' @family tasks
#' @export
print.task <- function(x, ...) {
  cat(format(x, ...), "\n")
}

#' Create a task to install a package and dependencies
#'
#' @param ... Additional parameters passed to [`task()`]
#' @param lib Any object that can be passed to [`lib()`] to generate a library
#'   path.
#' @inheritParams utils::install.packages
#'
#' @family tasks
#' @export
install_task <- function(
  origin,
  type = package_install_type(origin),
  INSTALL_opts = NULL,
  lib = lib_path(origin),
  ...
) {
  task(
    origin = origin,
    type = type,
    INSTALL_opts = INSTALL_opts,
    lib = lib,
    ...,
    .subclass = "install"
  )
}

#' @export
lib.install_task <- function(x, ...) {
  lib(x$lib, dir_hash = hash(x$origin, n = 8), name = package(x), ...)
}

is_type <- function(x, type) {
  UseMethod("is_type")
}

#' @export
is_type.default <- inherits

#' @export
is_type.list <- function(x, type) {
  vlapply(x, is_type, type = type)
}

#' @export
is_type.task <- function(x, type) {
  inherits(x, paste0(type, "_task"))
}

#' @export
is_type.process <- function(x, type) {
  inherits(x, paste0(type, "_process"))
}

is_install <- function(x) {
  is_type(x, "install")
}

is_check <- function(x) {
  is_type(x, "check")
}

is_actionable_task <- function(x) {
  is_type(x, "check") | is_type(x, "install")
}

is_meta <- function(x) {
  is_type(x, "meta")
}

#' Create a task to run `R CMD check`
#'
#' @inheritParams rcmdcheck::rcmdcheck
#' @inheritDotParams task
#'
#' @family tasks
#' @export
check_task <- function(build_args = NULL, args = NULL, env = NULL, ...) {
  task(
    env = env,
    args = args,
    build_args = build_args,
    ...,
    .subclass = "check"
  )
}

#' @export
lib.check_task <- function(x, ...) {
  character(0L)  # no additional libraries needed for checking
}

is_check_task <- function(x) {
  inherits(x, "check_task")
}

package <- function(x) {
  UseMethod("package")
}

#' @export
package.default <- function(x) {
  stop("Unrecognized type")
}

#' @export
package.task <- function(x) {
  package(x$origin)
}

#' @export
package.pkg_origin <- function(x) {
  x$package
}
