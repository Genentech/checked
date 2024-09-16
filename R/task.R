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
task <- function(...) {
  structure(list(...), class = "task")
}

#' @family tasks
#' @export
lib.task <- function(x, ...) {
  stop("Don't know how to determine a library path for generic task")
}

#' @family tasks
#' @export
print.task <- function(x, ...) {
  cat(format(x, ...), "\n")
}

#' @export
format.task <- function(x, ..., indent = 0L) {
  paste(collapse = "\n", c(
    paste0(strrep(" ", indent * 2), "<", friendly_name(x), ">"),
    vcapply(x$tasks, function(xi) format(xi, ..., indent = indent + 1))
  ))
}

#' @export
friendly_name.task <- function(x) {
  "task"
}

friendly_name <- function(x) {
  UseMethod("friendly_name")
}

#' @export
friendly_name.default <- function(x) {
  stop(
    "Dont' know how to name object with class(es) `",
    deparse(class(x)),
    "`"
  )
}

#' @family tasks
#' @export
subtasks <- function(name, tasks, ...) {
  task <- task(name = name, tasks = tasks, ...)
  class(task) <- c("subtasks_task", class(task))
  task
}

#' Attach Subtasks to an Existing Task
#'
#' @family tasks
#' @export
with_subtasks <- function(task, tasks) {
  task$tasks <- tasks
  class(task) <- c("subtasks_task", class(task))
  task
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
  task <- task(origin = origin, ...)
  task$type <- type
  task$INSTALL_opts <- INSTALL_opts
  task$lib <- lib
  class(task) <- c("install_task", class(task))
  task
}

#' @export
lib.install_task <- function(x, ...) {
  lib(x$lib, name = hash(x), ...)
}

#' @export
friendly_name.install_task <- function(x, ...) {
  paste0("install ", format(x$origin))
}

is_install_task <- function(x) {
  inherits(x, "install_task")
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
  task <- task(...)
  task$env <- env
  task$args <- args
  task$build_args <- build_args
  class(task) <- c("check_task", class(task))
  task
}

#' @export
lib.check_task <- function(x, ...) {
  character(0L)  # no additional libraries needed for checkign
}

is_check_task <- function(x) {
  inherits(x, "check_task")
}

#' @family tasks
#' @export
friendly_name.check_task <- function(x, ...) {
  paste0("check ", format(x$origin))
}

friendly_name.subtasks_task <- function(x, ...) {
  if (!is.null(x$name)) x$name else NextMethod()
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
