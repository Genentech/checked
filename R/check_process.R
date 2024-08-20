# Regular Expression for Parsing R CMD check checks
# nolint start, styler: off
RE_CHECK <- paste0(
  "(?<=^|\n)",       # starts on a new line (or start of string)
  "\\* checking ",   # literal "* checking "
  "(?<check>.*?)",   # capture any check content as "check"
  " \\.\\.\\.",      # until literal "..."
  "(?:",             # ignore additional check details:
    "[\\s\\*]{2,}",  #   any content starting with two or more of spaces or "*"
    ".*?(?:\n|$)",   #   until a newline (or end of string)
  ")*",              #   repeating until
  "(?<status>.*?)",  # capturing a status as "status"
  "(?=\n|$)"         # terminated by a new line (or end of string)
)
# nolint end, styler: on

DEFAULT_R_CMD_CHECK_VARIABLES <- c( # nolint
  "_R_CHECK_FORCE_SUGGESTS_" = FALSE,
  "_R_CHECK_RD_XREFS_" = FALSE,
  "_R_CHECK_SYSTEM_CLOCK_" = FALSE,
  "_R_CHECK_SUGGESTS_ONLY_" = TRUE
)

DEFAULT_BUILD_ARGS <- c( # nolint
  "--no-build-vignettes",
  "--no-manual"
)

DEFAULT_CHECK_ARGS <- c( # nolint
  "--timings",
  "--ignore-vignettes",
  "--no-manual"
)

#' @importFrom R6 R6Class
#' @importFrom rcmdcheck rcmdcheck_process
check_process <- R6::R6Class(
  "check_process",
  inherit = rcmdcheck::rcmdcheck_process,
  active = list(
    checks = function() {
      self$poll_output()
      private$parsed_checks
    }
  ),
  public = list(
    initialize = function(...) {
      args <- as.list(match.call(rcmdcheck::rcmdcheck, expand.dots = TRUE)[-1])
      args <- lapply(args, eval, envir = parent.frame())

      private$args <- args
      private$throttle <- throttle()
      private$spinners <- list(
        check = silent_spinner("circleHalves"),
        starting = silent_spinner(list(frames = c("\u2834", "\u2826", "\u2816", "\u2832")))
      )

      super$initialize(...)
    },
    set_finisher = function(callback) {
      private$finish_callback <- callback
      if (!self$is_alive()) callback(self)
    },
    finish = function() {
      self$poll_output()
      self$save_results()
      if (is.function(f <- private$finish_callback)) f(self)
    },
    get_time_last_check_start = function() {
      private$time_last_check_start
    },
    get_duration = function() {
      (self$get_time_finish() %||% Sys.time()) - self$get_start_time()
    },
    get_time_finish = function() {
      private$time_finish
    },
    get_checks = function() {
      self$poll_output()
      private$parsed_checks
    },
    spin = function() {
      if (length(private$parsed_checks) == 0) {
        private$spinners[["starting"]]$spin()
      } else if (self$is_alive()) {
        private$spinners[["check"]]$spin()
      } else {
        ""
      }
    },
    poll_output = function() {
      if (private$throttle()) {
        return()
      }

      if (!self$is_alive()) {
        private$time_last_check_start <- NULL
        private$time_finish <- private$time_finish %||% Sys.time()
      }

      # TODO: For some reason we need to read the output twice, otherwise
      # it might not be captured.
      # When forcing interruption, finisher is called, hence try() to make
      # sure it does not break after process is killed
      out <- try(paste0(
        private$parsed_partial_check_output,
        paste(super$read_output_lines(), collapse = "\n"),
        paste(super$read_output_lines(), collapse = "\n")
      ), silent = TRUE)

      if (inherits(out, "try-error")) {
        return()
      }

      captures <- checks_capture(out)
      checks <- checks_simplify(captures)

      if (length(checks) > 0) {
        private$time_last_check_start <- Sys.time()
        unknown <- !checks %in% levels(private$parsed_checks)
        checks[unknown] <- "NONE"
        private$parsed_checks[names(checks)] <- checks
      }

      if (length(private$parsed_checks) == 0) {
        # no checks were parsed
        private$parsed_partial_check_output <- out
      } else if (identical(unname(tail(checks, 1)), "")) {
        # the most recent output's check is still running
        n <- nrow(captures)
        private$parsed_partial_check_output <- captures[n, 1]
      } else {
        # the final check was fully parsed
        private$parsed_partial_check_output <- ""
      }
    },
    get_r_exit_status = function() {
      as.integer(inherits(try(self$get_result(), silent = TRUE), "try-error"))
    },
    save_results = function() {
      path <- file.path(private$check_dir, "result.json")
      try(rcmdcheck_to_json(self$parse_results(), path), silent = TRUE)
    }
  ),
  private = list(
    args = list(),
    time_last_check_start = NULL,
    time_finish = NULL,
    parsed_checks = factor(levels = c("", "NONE", "OK", "NOTE", "WARNING", "ERROR")),
    parsed_partial_check_output = "",
    throttle = NULL,
    spinners = NULL,
    finish_callback = NULL
  )
)

#' Generate A Rate Limiting Throttle Function
#'
#' @param interval An interval (in seconds) that is the minimum interval
#'   before [throttle] will return `TRUE`.
#' @return A throttling function with the provided interval. When called,
#'   returns a logical value indicating whether the throttle interval has
#'   passed (TRUE if the interval has not yet passed).
#'
#' @keywords internal
throttle <- function(interval = 0.2) {
  e <- environment()
  e$since <- Sys.time()
  function(since = e$since, interval = e$interval) {
    if (Sys.time() - since < interval) {
      TRUE
    } else {
      e$since <- Sys.time()
      FALSE
    }
  }
}

#' Parse R CMD checks from a partial check output string
#'
#' @param x A string, compsoed of any subsection of R CMD check console output
#' @return A matrix of matches and capture groups "check" and "status"
#'   ("OK", "NONE", "NOTE", "WARNING" or "ERROR").
#'
#' @examples
#' \dontrun{
#' check_output <- "
#' * checking check one ... OK
#' * checking check two ... NOTE
#' * checking tests ...
#'   Running test_abc.R
#'   Running test_xyz.R
#'  NONE
#' * checking check three ... WARNING
#' * ch
#' "
#'
#' checks_capture(check_output)
#' }
#'
#' @keywords internal
checks_capture <- function(x) {
  m <- gregexec(RE_CHECK, x, perl = TRUE)[[1]]

  if (!is.matrix(m)) {
    captures <- c("check", "status")
    return(t(matrix(character(0L), nrow = 3, dimnames = list(c("", captures)))))
  }

  # extend boundaries of input text to length of text
  l <- attr(m, "match.length")
  l[1, 1] <- l[1, 1] + m[1, 1]
  m[1, 1] <- 0
  l[1, ncol(l)] <- nchar(x) - m[1, ncol(m)] + 1

  t(matrix(
    substring(x, m, m + l - 1),
    nrow = nrow(m),
    ncol = ncol(m),
    dimnames = dimnames(m)
  ))
}

#' Simplify Captures into Vector
#'
#' @param x Matrix of regex captures as produced by [checks_capture].
#' @return A vector of check status, with names indicating the check
#'
#' @keywords internal
checks_simplify <- function(x) {
  checks <- trimws(x[, "status"])
  names(checks) <- trimws(x[, "check"])
  checks
}
