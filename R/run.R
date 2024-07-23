#' Run a Series of `R CMD check`s
#'
#' @param design `character` or `check_design` If a `character` value is
#'   provided, it is first coerced into a `check_design` using
#'   [`new_rev_dep_check_design()`].
#' @param ... Additional arguments passed to [`new_rev_dep_check_design()`]
#' @param reporter A reporter to provide progress updates. Will default to the
#'   most expressive command-line reporter given your terminal capabilities.
#'
#' @export
run <- function(design, ..., reporter = reporter_default()) {
  UseMethod("run")
}

#' @export
run.character <- function(design, ..., reporter = reporter_default()) {
  run(new_rev_dep_check_design(design, ...), reporter = reporter)
}

#' @export
run.check_design <- function(design, ..., reporter = reporter_default()) {
  on.exit(add = TRUE, {
    design$kill_all()
    report_finalize(reporter, design)
  })

  report_initialize(reporter, design)
  while (design$start_next_task() >= 0) {
    report_status(reporter, design)
    report_sleep(reporter, design)
  }

  invisible(design)
}
