#' A simple alternative to `devtools::as.package`
#'
#' Functionally identical to `devtools`' `as.package`, but without interactive
#' options for package creation.
#'
#' Function required for communicating with the `remotes` package interface
#'
#' @note Code inspired by `devtools` `load_pkg_description` with very minor
#' edits to further reduce `devtools` dependencies.
#'
#' @param x A package object to coerce
#' @keywords internal
as.package.remotes <- function(x) {
  if (inherits(x, "package")) {
    return(x)
  }
  info <- read.dcf(file.path(x, "DESCRIPTION"))[1L, ]
  Encoding(info) <- "UTF-8"
  desc <- as.list(info)
  names(desc) <- tolower(names(desc))
  desc$path <- x
  structure(desc, class = "package")
}


.remotes <- function() {
  if (requireNamespace("remotes", quietly = TRUE)) {
    as.list(getNamespace("remotes"), all.names = TRUE)[c(
      "extra_deps"
    )]
  } else {
    "remotes package not available"
  }
}
