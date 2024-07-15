#' Run Reverse-Dependency Checks
#'
#' @param design A reverse-dependency plan, or an object coercible into a
#'   plan.
#' @param ... Additional arguments
#' @param reporter A reporter to provide progress updates. Will default to the
#'   most expressive command-line reporter given your terminal capabilities.
#'
#' @export
run <- function(design, ..., reporter = default_reporter()) {
  UseMethod("run")
}

#' @export
run.character <- function(design, ..., reporter = default_reporter()) {
  run(new_rev_dep_check_design(design, ...), reporter = reporter)
}

#' @export
run.check_design <- function(design, ..., reporter = default_reporter()) {
  on.exit(add = TRUE, {
    design$terminate()
    report_finalize(reporter, design)
  })

  report_initialize(reporter, design)
  while (design$start_next_task() >= 0) {
    report_status(reporter, design)
    report_sleep(reporter, design)
  }

  invisible(design)
}
