#' Available Packages
#'
#' Functionally Equivalent to [`utils::available.packages()`], assuming
#' `utils`'s cache doesn't expire in the middle of a top-level callback.
#' Modifications were made so that the results for queries with unique
#' arguments are only called once for each top-level expression.
#'
#' Though [`utils::available.packages()`] will cache the `PACKAGES` index,
#' it must still be parsed with each call. Since this can happen hundreds of
#' times when building a `R CMD check` plan, this can cause a signficiant
#' bottleneck to the responsiveness of this package.
#'
#'     system.time({ for (i in 1:10) available.packages() })
#'     #>    user  system elapsed
#'     #>   3.453   0.196   3.655
#'
#'     system.time({ for (i in 1:10) available_packages() })
#'     #>    user  system elapsed
#'     #>   0.003   0.000   0.003
#'
#' @keywords internal
available_packages <- local({
  callback_name <- paste0(packageName(), "-last-top-level-time")
  callback_time <- NULL

  callback <- function(...) {
    last_top_level_callback <<- Sys.time()
    FALSE
  }

  maybe_add_callback <- function() {
    if (!callback_name %in% getTaskCallbackNames()) {
      addTaskCallback(name = callback_name, callback)
    }
  }

  memoise::memoise(
    hash = function(x) rlang::hash(list(x, callback_time)),
    function(...) {
      maybe_add_callback()
      utils::available.packages(...)
    }
  )
})
