#' Initialize a Reporter Object
#'
#' Reporters are primarily used as a data-less class for dispatch. They are
#' initialized with an empty environment, which can be used to store
#' arbitrary state during reporting.
#'
#' @noRd
reporter <- function(type) {
  type <- paste0("reporter_", type)
  structure(new.env(parent = baseenv()), class = c(type, "reporter"))
}

default_reporter <- function() {
  if (cli::is_ansi_tty()) {
    reporter("ansi_tty")
  } else if (cli::is_dynamic_tty()) {
    reporter("basic_tty")
  } else {
    reporter("basic_tty")
  }
}

reporter_ansi_tty <- function() {
  reporter("ansi_tty")
}

reporter_dynamic_tty <- function() {
  reporter("dynamic_tty")
}

reporter_basic_tty <- function() {
  reporter("basic_tty")
}

# Provide a default sleep period between updates
report_sleep <- function(reporter, design, sleep) {
  UseMethod("report_sleep")
}

#' @export
report_sleep.default <- function(reporter, design, sleep = 1) {
  Sys.sleep(sleep)
}

report_initialize <- function(reporter, design, envir = parent.frame()) {
  UseMethod("report_initialize")
}

report_status <- function(reporter, design, envir = parent.frame()) {
  UseMethod("report_status")
}

report_finalize <- function(reporter, design) {
  UseMethod("report_finalize")
}
