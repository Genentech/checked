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
#' @export
#' @rdname package_spec
package_spec <- function(name = NULL, repos = NULL) {
  structure(
    list(
      name = name,
      repos = repos
    ),
    class = "package_spec"
  )
}

#' @export
#' @rdname package_spec
package_spec_source <- function(path = NULL, ...) {
  package_spec <- package_spec(...)
  package_spec["path"] <- list(path)
  class(package_spec) <- c("package_spec_source", class(package_spec))
  package_spec
}

#' @export
#' @rdname package_spec
package_spec_archive_source <- function(path = NULL, ...) {
  package_spec <- package_spec_source(path, ...)
  
  class(package_spec) <- c("package_spec_archive_source", class(package_spec))
  package_spec
}

get_package_spec_dependencies <- function(package_spec) {
  UseMethod("get_package_spec_dependencies")
}

#' @export
get_package_spec_dependencies.default <- function(package_spec) {
  NULL
}

#' @export
get_package_spec_dependencies.package_spec <- function(package_spec) {
  db <- utils::available.packages(repos = package_spec$repos)
  row <- db[package_spec$name, , drop = FALSE]
  row[, DB_COLNAMES, drop = FALSE]
}

#' @export
get_package_spec_dependencies.package_spec_source <- function(package_spec) {
  row <- as.data.frame(read.dcf(file.path(package_spec$path, "DESCRIPTION")))
  row <- row[, intersect(DB_COLNAMES, colnames(row)), drop = FALSE]
  row[setdiff(DB_COLNAMES, colnames(row))] <- NA_character_
  row
}

#' @export
get_package_spec_dependencies.package_spec_archive_source <- function(package_spec) {
  path <- if (!file.exists(package_spec$path)) {
    fetch_package_source(package_spec$path, path_sources())
  } else {
    package_spec$path
  }
  utils::untar(path, exdir = dir)
  package_spec$path <- file.path(path, package_spec$name)
  get_package_spec_dependencies.package_spec_source(package_spec)
}


install_parameters <- function(package_spec) {
  UseMethod("install_parameters")
}

#' @export
install_parameters.package_spec <- function(package_spec) {
  list(package = package_spec$name, repos = package_spec$repos)
}

#' @export
install_parameters.package_spec_source <- function(package_spec) {
  list(package = package_spec$path, repos = NULL)
}

#' @export
install_parameters.package_spec_archive_source <- function(package_spec) {
  list(package = package_spec$path, repos = NULL)
}

check_path <- function(package_spec, ...) {
  UseMethod("check_path")
}

#' @export
check_path.package_spec <- function(package_spec, output, ...) {
  get_package_source(package_spec$name, package_spec$repos, destdir = output)
}

#' @export
check_path.package_spec_source <- function(package_spec, ...) {
  package_spec$path
}

#' @export
check_path.package_spec_archive_source <- function(package_spec, ...) {
  package_spec$path
}
