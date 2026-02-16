#' @export
report_start_setup.NULL <- function(...) {}

#' @export
report_start_checks.NULL <- function(...) {}

#' @export
report_finalize.NULL <- function(...) {}

#' @export
report_status.NULL <- function(...) {}

#' @export
report_finalize.NULL <- function(...) {}

#' @export
report_step.NULL <- function(reporter, checker) {
  checker$start_next_task() >= 0
}

#' @export
report_sleep.NULL <- function(reporter, checker, sleep = 0) {
  Sys.sleep(sleep)
}
