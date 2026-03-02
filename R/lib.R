#' Make a Library Location
#'
#' A description of where packages should be installed. This object provides
#' necessary information to determine where a package should be installed.
#' lib_path method creates default path handlers for given pkg origin while
#' lib_path_x creates an actual object.
#'
#' @param x A [`pkg_origin()`] object used for default dispatch.
#' @param ... Additional values
#' @param .class An optional subclass, used primarily for dispatch.
#'
#' @family specs
lib_path <- function(x, ..., .class = c()) {
  UseMethod("lib_path")
}

#' @export
lib_path.default <- function(x, ..., .class = c()) {
  lib_path_default(.class = .class)
}

#' @export
lib_path.pkg_origin_repo <- function(x, ..., .class = c()) {
  lib_path_default(.class = .class)
}

#' @export
lib_path.pkg_origin_local <- function(x, ..., .class = c()) {
  lib_path_isolated(.class = .class)
}

#' @export
lib_path.pkg_origin_remote <- function(x, ..., .class = c()) {
  x <- sanitize_pkg_origin_remote(x)
  NextMethod()
}

#' @rdname lib_path
lib_path_default <- function(.class = c()) {
  structure(list(), class = c(.class, "lib_path_default"))
}

#' @rdname lib_path
lib_path_isolated <- function(.class = c()) {
  structure(list(), class = c(.class, "lib_path_isolated"))
}

#' @export
format.lib_path <- function(x, ...) {
  "library"
}

#' Get Library Location
#'
#' @param x An object describing a library location
#' @param ... additional parameters passed to methods
#' @param lib.loc Library paths, defaulting to [`.libPaths()`].
#' @param lib.root A root directory for the isolated library.
#' @param dir_hash unique identifier of the isolated library
#' @param name human-readable subname of the isolated library
#'
#' @keywords internal
lib <- function(x, ...) {
  UseMethod("lib")
}

#' @method lib NULL
#' @export
#' @rdname lib
lib.NULL <- function(x, ...) {
  character(0L)
}

#' @export
#' @rdname lib
lib.character <- function(x, ...) {
  x
}

#' @export
#' @rdname lib
lib.lib_path_isolated <- function(
  x,
  ...,
  lib.root = tempdir(),
  dir_hash = hash(Sys.time(), n = 8),
  name = ""
) {
  dirname <- if (nzchar(name)) {
    paste0(name, "-", dir_hash)
  } else {
    dir_hash
  }
  file.path(lib.root, dirname)
}

#' @export
#' @rdname lib
lib.lib_path_default <- function(
  x,
  ...,
  lib.loc = .libPaths()
) {
  # In most cases this assumes checker library was appended to the lib.loc
  lib.loc[[1]]
}
