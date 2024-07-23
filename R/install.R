#' @importFrom R6 R6Class
#' @importFrom callr r_process
install_packages_process <- R6::R6Class(
  "install_package_process",
  inherit = callr::r_process,
  public = list(
    log = NULL,
    initialize = function(pkgs, ..., lib = .libPaths(), libpaths = .libPaths(), log) {
      private$package <- pkgs
      self$log <- log
      private$callr_r_bg(
        function(...) {
          tryCatch(
            utils::install.packages(...)
            warning = function(w) {
              installation_failure <- grepl("download of package .* failed", w$message) ||
                grepl("(dependenc|package).*(is|are) not available", w$message) ||
                grepl("installation of package.*had non-zero exit status", w$message) ||
                grepl("installation of one or more packages failed", w$message)
              
              if (installation_failure) {
                stop(w$message)
              }
            }
          )
        },
        args = list(pkgs, ..., lib = lib),
        libpath = libpaths,
        stdout = log,
        stderr = "2>&1",
        system_profile = TRUE
      )
    },
    get_duration = function() {
      if (!self$is_alive() && is.null(private$time_finish)) {
        private$time_finish <- Sys.time()
      }
      (private$time_finish %||% Sys.time()) - self$get_start_time()
    },
    set_finalizer = function(callback) {
      private$finalize_callback <- callback
      if (!self$is_alive()) callback()
    },
    finalize = function() {
      private$time_finish <- Sys.time()
      if (is.function(f <- private$finalize_callback)) f(self)
      if ("finalize" %in% ls(super)) super$finalize()
    },
    get_r_exit_status = function() {
      as.integer(inherits(try(p$get_result(), silent = TRUE), "try-error"))
    }
  ),
  private = list(
    options = NULL,
    package = NULL,
    finalize_callback = NULL,
    time_finish = NULL,
    callr_r_bg = function(...) {
      # default formal argument values
      options <- formals(callr::r_bg)
      options <- options[vlapply(options, `!=`, bquote())]
      options <- lapply(options, eval, envir = asNamespace("callr"))

      # ellipsis arguments
      dots <- match.call(callr::r_bg, expand.dots = TRUE)[-1]
      dots <- lapply(dots, eval, envir = parent.frame())
      options[names(dots)] <- dots

      # other things set internally in callr::r_bg
      options$extra <- list()
      options$load_hook <- ""

      private$options <- options
      super$initialize(options = options)
    }
  )
)
