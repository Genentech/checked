#' Available Packages
#'
#' Functionally Equivalent to [`utils::available.packages()`], assuming
#' `utils`'s cache doesn't expire in the middle of a top-level call evaluation.
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
#'     #>   0.325   0.002   0.328
#'
#' @note This _could_ be removed by propagating the `available.packages()`
#'   database matrix through all the calls that need to use it, though this
#'   would be a sizable refactor.
#'
#' @keywords internal
available_packages <- local({
  callback_name <- paste0(packageName(), "-top-level-ap")
  callback_index <- 0L

  increment_index <- function(...) {
    callback_index <<- callback_index + 1L
    FALSE
  }

  maybe_add_callback <- function() {
    if (!callback_name %in% getTaskCallbackNames()) {
      addTaskCallback(name = callback_name, increment_index)
    }
  }

  memoise::memoise(
    hash = function(x) rlang::hash(list(x, callback_index)),
    function(...) {
      maybe_add_callback()
      utils::available.packages(...)
    }
  )
})
