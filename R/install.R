#' @importFrom utils packageName install.packages
#' @importFrom R6 R6Class
#' @importFrom callr r_process
install_process <- R6::R6Class(
  "install_process",
  inherit = callr::r_process,
  public = list(
    log = NULL,
    initialize = function(
      pkgs,
      ...,
      lib = .libPaths()[[1]],
      libpaths = .libPaths(),
      log = NULL,
      env = options::opt("install_envvars")
    ) {
      if (!dir.exists(lib)) dir.create(lib, recursive = TRUE)
      private$package <- pkgs
      self$log <- log
      private$callr_r_bg(
        function(..., opts_to_inherit) {
          do.call(options, opts_to_inherit)
          invisible(capture.output(withCallingHandlers(
            utils::install.packages(..., quiet = FALSE, verbose = TRUE),
            warning = function(w) {
              print(w$message)
            }
          ), split = TRUE))
        },
        args = list(
          private$package,
          ...,
          lib = lib,
          opts_to_inherit = do.call(
            options, options::opt("install_opts_to_inherit")
          )
        ),
        libpath = libpaths,
        stdout = self$log,
        stderr = "2>&1",
        system_profile = options::opt("install_system_profile"),
        user_profile = options::opt("install_user_profile"),
        cmdargs = c("--slave", "--no-save", "--no-restore", "--vanilla"),
        env = env
      )
    },
    get_duration = function() {
      if (!self$is_alive() && is.null(private$time_finish)) {
        private$time_finish <- Sys.time()
      }
      (private$time_finish %||% Sys.time()) - self$get_start_time()
    },
    set_finisher = function(callback) {
      private$finish_callback <- callback
      if (!self$is_alive()) callback(self)
    },
    finish = function() {
      private$time_finish <- Sys.time()
      private$free_file_descriptors()
      if (is.function(f <- private$finish_callback)) f(self)
    },
    get_r_exit_status = function() {
      res <- self$get_results_safe()
      if (inherits(self$get_results_safe(), "callr_error")) {
        1L
      } else if (any(is_install_failure_warning(res))) {
        1L
      } else {
        0L
      }

    },
    get_results_safe = function() {
      tryCatch(
        gsub("\\n", "\n", self$get_result(), fixed = TRUE),
        error = function(e) {
          e
        },
        warning = function(w) {
          w
        }
      )
    }
  ),
  private = list(
    options = NULL,
    package = NULL,
    finish_callback = NULL,
    time_finish = NULL,
    callr_r_bg = function(...) {
      # default formal argument values
      options <- formals(callr::r_bg)
      options <- options[vlapply(options, function(x) !identical(x, bquote()))]
      options <- lapply(options, eval, envir = asNamespace("callr"))

      # ellipsis arguments
      dots <- match.call(callr::r_bg, expand.dots = TRUE)[-1]
      dots <- lapply(dots, eval, envir = parent.frame())
      options[names(dots)] <- dots

      # other things set internally in callr::r_bg
      options$extra <- list()
      options$load_hook <- .callr$default_load_hook()

      private$options <- options
      super$initialize(options = options)
    },
    free_file_descriptors = function() {
      if (self$has_output_connection()) close(self$get_output_connection())
      if (self$has_error_connection())  close(self$get_error_connection())
      if (self$has_poll_connection())   close(self$get_poll_connection())
      if (self$has_input_connection())   close(self$get_input_connection())
    }
  )
)

is_install_failure_warning <- function(w) {
  patterns <- c(
    "download of package .* failed",
    "(dependenc|package).*(is|are) not available",
    "installation of package.*had non-zero exit status",
    "installation of one or more packages failed",
    "cannot open compressed file",
    "(C|c)ouldn't connect to server",
    "Timeout.*was reached"
  )

  re <- paste0("(", paste0(patterns, collapse = "|"), ")")
  grepl(re, w)
}
