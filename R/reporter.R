#' Check Design Runner Reporters
#'
#' Reporters are used to configure how output is communicated while running
#' a [`checker`]. They range from glossy command-line tools intended for
#' displaying progress in an interactive R session, to line-feed logs which
#' may be better suited for automated execution, such as in continuous
#' itegration.
#'
#' ## [`reporter_default()`]
#'
#' Automatically chooses an appropriate reporter based on the calling context.
#'
#' ## [`reporter_ansi_tty()`]
#'
#' Highly dynamic output for fully capable terminals. Requires multi-line
#' dynamic output, which may not be available in editors that that present a
#' terminal as a web component.
#'
#' ## [`reporter_basic_tty()`]
#'
#' A line-feed reporter presenting output one line at a time, providing
#' a reporter with minimal assumptions about terminal capabilities.
#'
#' @family reporters
#' @name reporters
NULL

reporter <- function(type) {
  type <- paste0("reporter_", type)
  structure(new.env(parent = baseenv()), class = c(type, "reporter"))
}

#' @rdname reporters
#' @export
reporter_ansi_tty <- function() {
  reporter("ansi_tty")
}

#' @rdname reporters
#' @export
reporter_ansi_tty2 <- function() {
  reporter("ansi_tty2")
}

#' @rdname reporters
#' @export
reporter_basic_tty <- function() {
  reporter("basic_tty")
}

#' @rdname reporters
#' @export
reporter_default <- function() {
  if (cli::is_ansi_tty()) {
    reporter_ansi_tty()
  } else if (cli::is_dynamic_tty()) {
    reporter_basic_tty()
  } else {
    reporter_basic_tty()
  }
}

#' Reporter Internal Methods
#'
#' Each of the internal methods for reporters take a reporter, the check
#' design object and a calling environment.
#'
#' @param reporter A object produced using [`reporters`]. Each reporter is a
#'   thin wrapper around an environment with a class name for dispatch. The
#'   reporter is mutable and captures any necessary state that needs to be
#'   tracked while reporting.
#' @param design [`checker`] The check design to report as it evaluates.
#' @param envir `environment` An environment to attach to, to leverage on-exit
#'   hooks.
#' @param sleep `numeric` An interval to pause between reporter steps.
#'
#' @name reporters-internal
#' @rdname reporters-internal
#' @keywords internal
NULL

#' @rdname reporters-internal
report_sleep <- function(reporter, design, sleep) {
  UseMethod("report_sleep")
}

#' @rdname reporters-internal
#' @export
report_sleep.default <- function(reporter, design, sleep = 1) {
  Sys.sleep(sleep)
}

#' @rdname reporters-internal
report_initialize <- function(reporter, design, envir = parent.frame()) {
  UseMethod("report_initialize")
}

#' @rdname reporters-internal
report_status <- function(reporter, design, envir = parent.frame()) {
  UseMethod("report_status")
}

#' @rdname reporters-internal
report_finalize <- function(reporter, design) {
  UseMethod("report_finalize")
}
