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
#'
meta_task <- function(..., .subclass = NULL) {
  task(..., .subclass = c(sprintf("%s_meta", .subclass), "meta"))
}

make_unique_task <- function(task, seed = runif(1)) {
  task$id <- hash(seed)
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
  type = getOption("pkgType"),
  INSTALL_opts = NULL,
  lib = lib_loc_default(),
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
  lib(x$lib, name = hash(x), ...)
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
  character(0L)  # no additional libraries needed for checkign
}

is_check_task <- function(x) {
  inherits(x, "check_task")
}

#' Specify a library install
#'
#' A declarative task that specifies a set of packages that should be installed
#' to a given location. This task is typically used during planning of checks,
#' but does not spawn a process. It is used primarily for organizing the
#' library precedence for check tasks.
#'
library_task <- function(origins = list(), loc = lib_loc_default(), ...) {
  task <- task(...)
  task$origins <- origins
  task$loc <- loc
  class(task) <- c("library_task", class(task))
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

# TODO:
#   Convert plan to graph
#
#   This can probably be avoided if we were to build a task graph from
#   the start. Plans may be better represented as declarative graphs, which
#   are then hydrated into imparative graphs. Then the hierarchical
#   relationships are already encoded in the graph and need not be
#   rediscovered by building a dependency graph post-hoc.
#
as_desc <- function(x, ...) {
  UseMethod("as_desc")
}

#' @export
as_desc.default <- function(x, ...) {
  NULL
}

#' @export
as_desc.subtasks_task <- function(x, ...) {
  descs <- list()
  length(descs) <- length(x$tasks)

  for (i in seq_along(x$tasks)) {
    descs[[i]] <- as_desc(x$tasks[[i]], ...)
  }

  descs <- bind_descs(descs)

  # if this subtask is also a task of itself, add to descriptions collection
  # after substituting aliased package names in dependencies
  task_desc <- NextMethod()
  if (!is.null(task_desc)) {
    descs <- bind_descs(list(task_desc, descs))
    descs <- sub_desc_aliases(descs)
  }

  descs
}

#' @export
as_desc.install_task <- function(x, ...) {
  cbind(as_desc(x$origin), Task = hash(x))
}

#' @export
as_desc.check_task <- function(x, ...) {
  cbind(as_desc(x$origin), Task = hash(x))
}

#' @export
as_desc.character <- function(x) {
  desc <- read.dcf(x)
  read.dcf(x, fields = unique(c(colnames(desc), names(DEP))))
}

#' @export
as_desc.pkg_origin_local <- function(x) {
  desc <- as_desc(file.path(x$source, "DESCRIPTION"))

  # update the Package field, replacing actual name with an alias
  desc <- cbind(desc, Alias = x$package)
  rownames(desc) <- desc[, "Package"] <- hash(x)

  desc
}

#' @export
as_desc.pkg_origin_repo <- function(x) {
  ap <- available_packages(repos = x$repos)
  ap[x$package, , drop = FALSE]
}

flatten <- function(x) {
  UseMethod("flatten")
}

#' @export
flatten.default <- function(x) {
  list(x)
}

#' @export
flatten.subtasks_task <- function(x) {
  unlist(recursive = FALSE, c(
    list(list(x)),
    lapply(x$tasks, function(xi) flatten(xi))
  ))
}
