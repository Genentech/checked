#' Task specification
#' 
#' Create task specification list which consists of all the details required
#' to run specific task.
#' 
#' @param alias task alias which also serves as unique identifier of the task.
#' @param package_spec \code{\link[checked]{package_spec}} object
#' @param env environmental variables to be set in separate process running 
#' specific task.
#' @param revdep character indicating whether the task specification describes
#' check associated with the development (new) or release (old) version of the
#' for which reverse dependency check is run.
#' @param ... parameters passed to downstream constructors
#' @inheritParams utils::install.packages
#' @inheritParams rcmdcheck::rcmdcheck
#' 
#' @export
#' @rdname task_spec
task_spec <- function(alias = NULL, package_spec = NULL, env = NULL) {
  structure(
    list(
      alias = alias,
      package_spec = package_spec,
      env = env
    ),
    class = "task_spec"
  )
}

list_of_task_spec <- function(x, ...) {
  structure(x, class = c("list_of_task_spec", "list"))
}

#' @export
print.task_spec <- function(x, ...) {
  cat(format(x, ...), "\n")
}

#' @export
format.task_spec <- function(x, ...) {
  paste0("<task ", x$alias, ">")
}

#' @export
format.list_of_task_spec <- function(x, ...) {
  vcapply(x, format)
}

#' @export
#' @rdname task_spec
install_task_spec <- function(type = getOption("pkgType"), INSTALL_opts = NULL, ...) {
  task_spec <- task_spec(...)
  install_spec <- list(
    type = type,
    INSTALL_opts = INSTALL_opts
  )
  structure(
    c(install_spec, task_spec),
    class = c("install_task_spec", class(task_spec))
  )
}

#' @export
#' @rdname task_spec
custom_install_task_spec <- function(...) {
  task_spec <- install_task_spec(...)
  
  class(task_spec) <- c("custom_install_task_spec", class(task_spec))
  task_spec
}

#' @export
#' @rdname task_spec
check_task_spec <- function(args = NULL, build_args = NULL, ...) {
  task_spec <- task_spec(...)
  check_spec <- list(
    args = args,
    build_args = build_args
  )
  
  structure(
    c(check_spec, task_spec),
    class = c("check_task_spec", class(task_spec))
  )
}

#' @export
#' @rdname task_spec
revdep_check_task_spec <- function(revdep, ...) {
  task_spec <- check_task_spec(...)
  task_spec["revdep"] <- list(revdep)
  class(task_spec) <- c("revdep_check_task_spec", class(task_spec))

  task_spec
}
