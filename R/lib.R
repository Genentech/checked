#' Make a Library Location
#'
#' A description of where packages should be installed. This object provides
#' necessary information to determine where a package should be installed.
#'
#' @param x Any additional metadata that can be prescribed when describing
#'   a library location and used when building the path. Defaults to an empty
#'   [`list()`].
#' @param .class An optional subclass, used primarily for dispatch.
#'
lib_loc <- function(x = list(), .class = c()) {
  structure(x, class = c(.class, "lib_loc"))
}

lib_loc_default <- function() {
  lib_loc(.class = "lib_loc_default")
}

lib_loc_isolated <- function() {
  lib_loc(.class = "lib_loc_isolated")
}

#' Get Library Location
#'
#' @param x An object describing a library location
#' @param lib.loc A set of library locations, used as defaults for objects
#'   that may make use of them.
#'
lib <- function(x, ..., lib.loc = c()) { # nolint: object_name_linter.
  UseMethod("lib")
}

#' Null Library Path
#' 
#' @export
lib.NULL <- function(x, ...) {
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
#' @param x A `lib_loc_isolated` object.
#' @param name A name for the library, defaults to a random hash.
#' @param lib.root A root directory for the isolated library.
#' @param ... Additional arguments unused
#'
#' @export
lib.lib_loc_isolated <- function(
  x,
  name = rlang::hash(runif(1)),
  lib.root = tempdir(),
  ...
) {
  file.path(lib.root, name)
}

#' Produce a Default Library Path
#'
#' @param x A `lib_loc_default` object.
#' @param ... Additional arguments unused
#' @param lib.loc Library paths, defaulting to [`.libPaths()`].
#'
#' @export
lib.lib_loc_default <- function(
  x,
  ...,
  lib.loc = .libPaths()
) {
  lib.loc[[1]]
}
