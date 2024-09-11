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
#'
#' @family tasks
#' @export
task <- function(...) {
  structure(list(...), class = "task")
}

hash_task <- function(x, n = 12) {
  substring(cli::hash_obj_sha256(x), 1, n)
}

#' @family tasks
#' @export
print.task <- function(x, ...) {
  cat(format(x, ...), "\n")
}

#' @family tasks
#' @export
format.task <- function(x, ...) {
  if (is.list(x)) {
    fmt_tasks <- paste0(vcapply(x, function(xi) format(xi)), collapse = ", ")
    return(paste0("[", fmt_tasks, "]"))
  }

  NextMethod()
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
  origin,
  type = getOption("pkgType"),
  INSTALL_opts = NULL, # nolint: object_name_linter.
  ...
) {
  task <- task(origin = origin, ...)
  task$type <- type
  task$INSTALL_opts <- INSTALL_opts
  class(task) <- c("install_task", class(task))
  task
}

is_install_task <- function(x) {
  inherits(x, "install_task")
}

#' @family tasks
#' @export
format.install_task <- function(x, ...) {
  paste0("<install ", format(x$origin), ">")
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

is_check_task <- function(x) {
  inherits(x, "check_task")
}

#' @family tasks
#' @export
format.check_task <- function(x, ...) {
  paste0("<check ", format(x$origin), ">")
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

tasks <- function(x) {
  structure(x, class = "tasks")
}

#' @export
format.tasks <- function(x, ...) {
  elems <- lapply(x, function(xi) format(xi, ...))
  paste0("[", paste0(elems, collapse = ", "), "]")
}

as_desc <- function(x, ...) {
  UseMethod("as_desc")
}

#' @export
as_desc.list <- function(x, ...) {
  descs <- list()
  length(descs) <- length(x)

  for (i in seq_along(x)) {
    descs[[i]] <- as_desc(x[[i]])
  }

  descs <- bind_descs(descs)
  descs <- sub_aliased_desc(descs)
  descs
}

#' @export
as_desc.install_task <- function(x, ...) {
  cbind(as_desc(x$origin), Task = hash_task(x))
}

#' @export
as_desc.check_task <- function(x, ...) {
  cbind(as_desc(x$origin), Task = hash_task(x))
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
  rownames(desc) <- desc[, "Package"] <- hash_task(x)

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
flatten.list <- function(x) {
  if (!inherits(x, "list")) return(x)
  unlist(recursive = FALSE, lapply(x, function(xi) flatten(xi)))
}

