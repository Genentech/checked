#' Package specification
#'
#' Create package specification list which consists of all the details required
#' to identify and acquire source of the package.
#'
#' @param name name of the package.
#' @param repos repository where package with given name should identified.
#' @param path path to the source of the package (either bundled or not). URLs
#' are acceptable.
#' @param ... parameters passed to downstream constructors
#'
#' @family specs
#' @export
pkg_origin <- function(name, ..., .class = c()) {
  structure(list(name = name, ...), class = c(.class, "pkg_origin_source"))
}

#' @export
#' @rdname pkg_origin
pkg_origin_repo <- function(repos, ...) {
  pkg_origin(..., repos = repos, .class = "pkg_origin_repo")
}

#' @export
#' @rdname pkg_origin
pkg_origin_local <- function(path = NULL, ...) {
  pkg_origin(..., path = path, .class = "pkg_origin_local")
}

#' @export
#' @rdname pkg_origin
pkg_origin_archive <- function(path = NULL, ...) {
  pkg_origin(..., path = path, .class = "pkg_origin_archive")
}

pkg_deps <- function(x) {
  UseMethod("pkg_deps")
}

#' @export
pkg_deps.default <- function(x) {
  NULL
}

#' @export
pkg_deps.pkg_origin_source <- function(x) {
  db <- utils::available.packages(repos = x$repos)
  row <- db[x$name, , drop = FALSE]
  row[, DB_COLNAMES, drop = FALSE]
}

#' @export
pkg_deps.pkg_origin_local <- function(x) {
  row <- as.data.frame(read.dcf(file.path(x$path, "DESCRIPTION")))
  row <- row[, intersect(DB_COLNAMES, colnames(row)), drop = FALSE]
  row[setdiff(DB_COLNAMES, colnames(row))] <- NA_character_
  row
}

#' @export
pkg_deps.pkg_origin_archive <- function(x) {
  path <- if (!file.exists(x$path)) {
    fetch_package_source(x$path, path_sources())
  } else {
    x$path
  }
  utils::untar(path, exdir = dir)
  x$path <- file.path(path, x$name)
  pkg_deps.pkg_origin_local(x)
}


install_params <- function(x) {
  UseMethod("install_params")
}

#' @export
install_params.pkg_origin_source <- function(x) {
  list(package = x$name, repos = x$repos)
}

#' @export
install_params.pkg_origin_local <- function(x) {
  list(package = x$path, repos = NULL)
}

#' @export
install_params.pkg_origin_archive <- function(x) {
  list(package = x$path, repos = NULL)
}

check_path <- function(package_source, ...) {
  UseMethod("check_path")
}

#' @export
check_path.pkg_origin <- function(x, output, ...) {
  stop(sprintf("Can't determine origin of package '%s'", x$name))
}

#' @export
check_path.pkg_origin_repo <- function(x, output, ...) {
  get_package_source(x$name, x$repos, destdir = output)
}

#' @export
check_path.pkg_origin_local <- function(x, ...) {
  x$path
}

#' @export
check_path.pkg_origin_archive <- function(x, ...) {
  x$path
}
