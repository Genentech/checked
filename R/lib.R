#' Make a Library Location
#'
#' A description of where packages should be installed. This object provides
#' necessary information to determine where a package should be installed.
#' lib_path method creates default path handlers for given pkg origin while
#' lib_path_x creates an actual object.
#'
#' @param x A [`pkg_origin()`] object
#' @param ... Additional values
#' @param .class An optional subclass, used primarily for dispatch.
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
  lib_path_isolated(origin = x, .class = .class)
}

#' @export
lib_path.pkg_origin_remotes <- function(x, ..., .class = c()) {
  x <- sanitize_pkg_origin_remotes(x)
  NextMethod()
}

#' @rdname lib_path
lib_path_default <- function(.class = c()) {
  structure(list(), class = c(.class, "lib_path_default"))
}

#' @rdname lib_path
lib_path_isolated <- function(..., .class = c()) {
  structure(list(...), class = c(.class, "lib_path_isolated"))
}

#' @export
format.lib_path <- function(x, ...) {
  "library"
}

#' Get Library Location
#'
#' @param x An object describing a library location
#' @param lib.loc A set of library locations, used as defaults for objects
#'   that may make use of them.
#'
lib <- function(x, ..., lib.loc = c()) {
  UseMethod("lib")
}

#' Null Library Path
#'
#' @export
lib.NULL <- function(x, ..., lib.loc = c()) {
  character(0L)
}

#' Produce a Library from a Path
#'
#' @param x A `character` object
#' @param ...,lib.loc Additional arguments unused
#'
#' @export
lib.character <- function(x, ..., lib.loc = c()) {
  x
}

#' Produce an Isolated Library Path
#'
#' @param x A `lib_path_isolated` object.
#' @param name A name for the library, defaults to a random hash.
#' @param lib.root A root directory for the isolated library.
#' @param ... Additional arguments unused
#'
#' @export
lib.lib_path_isolated <- function(
  x,
  ...,
  lib.root = tempdir(),
  lib.loc = c()
) {
  file.path(lib.root, paste0(x$origin$package, "-", hash(x$origin, n = 8)))
}

#' Produce a Default Library Path
#'
#' @param x A `lib_path_default` object.
#' @param ... Additional arguments unused
#' @param lib.loc Library paths, defaulting to [`.libPaths()`].
#'
#' @export
lib.lib_path_default <- function(
  x,
  ...,
  lib.loc = c()
) {
  # In most cases this assumes checker library was appended to the lib.loc
  lib.loc[[1]]
}
