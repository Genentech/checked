#' Run a Series of `R CMD check`s
#'
#' [`run()`] provides a generic, and is the central interface for executing
#' [`checker`]s. If a path is provided, a new reverse dependency check
#' plan is generated from the source code path. Otherwise a plan can be
#' built separately and executed using [`run()`].
#'
#' @param checks `character` or `checker` If a `character` value is
#'   provided, it is first coerced into a `checker` using
#'   [`new_rev_dep_checker()`].
#' @param ... Additional arguments passed to [`new_rev_dep_checker()`]
#' @param reporter A reporter to provide progress updates. Will default to the
#'   most expressive command-line reporter given your terminal capabilities.
#'
#' @export
run <- function(checker, ..., reporter = reporter_default()) {
  UseMethod("run")
}

#' @export
run.character <- function(checker, ..., reporter = reporter_default()) {
  run(new_rev_dep_checker(checker, ...), reporter = reporter)
}

#' @export
run.checker <- function(checker, ..., reporter = reporter_default()) {
  on.exit(add = TRUE, {
    checker$terminate()
    report_finalize(reporter, checker)
  })

  report_initialize(reporter, checker)
  while (checker$step()) {
    report_status(reporter, checker)
    report_sleep(reporter, checker)
  }

  invisible(checker)
}
