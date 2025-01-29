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
pkg_origin <- function(package, ..., .class = c()) {
  structure(list(package = package, ...), class = c(.class, "pkg_origin"))
}

fmt_pkg_origin_source <- function(x, ...) {
  if (is.null(x$source)) {
    ""
  } else if (is.null(names(x$source))) {
    format(x$source, ..., pretty = TRUE)
  } else {
    names(x$source)
  }
}

#' @export
format.pkg_origin <- function(x, ..., short = FALSE) {
  paste(collapse = " ", c(
    x$package,
    if (!short && !is.null(x$version)) {
      switch(class(x$version)[[1]],
        package_version = paste0("(v", format(x$version), ")"),
        x$version
      )
    },
    if (!short && !is.null(x$source)) {
      paste0("from ", fmt_pkg_origin_source(x))
    }
  ))
}

#' @export
#' @rdname pkg_origin
pkg_origin_repo <- function(package, repos, ...) {
  ap_pkg <- available_packages(repos = repos)[package, ]

  version <- package_version(ap_pkg["Version"])
  source <- strip_src_contrib(ap_pkg["Repository"])
  if (any(which <- startsWith(repos, source))) {
    source <- repos[which][1]
  }

  pkg_origin(
    package = package,
    version = version,
    source = source,
    repos = repos,
    ...,
    .class = "pkg_origin_repo"
  )
}


#' @export
#' @rdname pkg_origin
try_pkg_origin_repo <- function(package, repos, ...) {
  if (isTRUE(pkg_origin_is_base(package))) {
    return(pkg_origin_base(package, ...))
  } else if (package %in% available_packages(repos = repos)[, "Package"]) {
    pkg_origin_repo(package = package, repos = repos, ...)
  } else {
    pkg_origin_unknown(package = package, ...)
  }
}


#' @export
#' @rdname pkg_origin
pkg_origin_is_base <- function(package, ...) {
  package %in% installed.packages()[, "Package"] &&
    installed.packages()[package, "Priority"] == "base"
}


#' @export
#' @rdname pkg_origin
pkg_origin_base <- function(package, ...) {
  pkg_origin(
    package = package,
    .class = "pkg_origin_base"
  )
}


#' @export
#' @rdname pkg_origin
pkg_origin_unknown <- function(package, ...) {
  pkg_origin(
    package = package,
    .class = "pkg_origin_unknown"
  )
}

#' @export
#' @rdname pkg_origin
pkg_origin_local <- function(path = NULL, ...) {
  package <- get_package_name(path)
  version <- package_version(get_package_version(path))
  source <- filepath(normalizePath(path))

  pkg_origin(
    package = package,
    version = version,
    source = source,
    ...,
    .class = "pkg_origin_local"
  )
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
pkg_deps.pkg_origin_local <- function(x) {
  db <- available_packages(repos = x$repos)
  row <- db[x$package, , drop = FALSE]
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
install_params.pkg_origin <- function(x, output, ...) {
  stop(sprintf("Can't determine origin of package '%s'", x$name))
}

#' @export
install_params.pkg_origin_base <- function(x) {
  list()  # no installation needed, distributed with R
}

#' @export
install_params.pkg_origin_repo <- function(x) {
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
  get_package_source(x$package, x$repos, destdir = output)
}

#' @export
check_path.pkg_origin_local <- function(x, ...) {
  x$path
}

#' @export
check_path.pkg_origin_archive <- function(x, ...) {
  x$path
}

dep_tree <- function(x, ...) {
  UseMethod("dep_tree")
}

#' @export
dep_tree.check_task <- function(x, ...) {
  dep_tree(x$origin, ...)
}

#' @export
dep_tree.install_task <- function(x, ...) {
  dep_tree(x$origin, ...)
}

#' @export
dep_tree.pkg_origin <- function(
  x,
  ...,
  db = available_packages(),
  dependencies = TRUE
) {
  dep_tree(x$package, ..., db = db, dependencies = dependencies)
}

#' @export
dep_tree.character <- function(
  x,
  ...,
  db = available_packages(),
  dependencies = TRUE
) {
  df <- pkg_dependencies(x, dependencies = dependencies, db = db, ...)
  colmap <- c("package" = "from", "name" = "to")
  rename <- match(names(df), names(colmap))
  to_rename <- !is.na(rename)
  names(df)[to_rename] <- colmap[rename[to_rename]]
  df <- df[, c(which(to_rename), which(!to_rename)), drop = FALSE]
  igraph::graph_from_data_frame(df)
}
