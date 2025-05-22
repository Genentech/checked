filepath <- function(x) {
  structure(x, class = "filepath")
}

cli_type <- function(types, x) {
  structure(x, class = paste0("cli_", types))
}

#' @export
format_simplify_path <- function(x, ..., full.path = FALSE) {
  if (full.path) {
    return(normalizePath(x, mustWork = FALSE))
  }

  wp <- path_parts(getwd())
  xp <- path_parts(normalizePath(x, mustWork = FALSE))
  min_len <- min(length(wp), length(xp))
  first_diff <- Position(identity, head(wp, min_len) != head(xp, min_len))

  if (is.na(first_diff)) {
    parts <- utils::tail(xp, -min_len)
    if (length(parts) == 0) {
      parts <- "."
    }
    do.call(file.path, as.list(parts))
  }

  if (first_diff > min_len) {
    parts <- utils::tail(xp, -first_diff + 1)
    do.call(file.path, as.list(parts))
  } else if (first_diff <= min_len) {
    parents <- rep_len("..", length(xp) - first_diff + 1)
    parts <- c(parents, utils::tail(xp, -first_diff + 1))
    do.call(file.path, as.list(parts))
  } else {
    x
  }
}

#' Split a Filepath into Parts
#'
#' @param x A `character(1L)` or `filepath`
#' @return A `character` vector of path parts
#'
#' @keywords internal
path_parts <- function(x) {
  parts <- character()
  repeat {
    parts[[length(parts) + 1L]] <- basename(x)
    if (x == dirname(x)) break
    x <- dirname(x)
  }
  rev(parts)
}

path_default <- function() {
  file.path(tempdir(), utils::packageName())
}

path_libs <- function(path) {
  dir_create(p <- file.path(path, "libs"))
  normalizePath(p)
}

path_lib <- function(path) {
  dir_create(p <- file.path(path_libs(path), "lib"))
  normalizePath(p)
}

path_custom_lib <- function(path, custom) {
  valid_name <- unique_alias(custom)
  dir_create(p <- file.path(path_libs(path), valid_name))
  normalizePath(p)
}

path_package_install_log <- function(path, package, name = "lib") {
  dir_create(p <- file.path(path_logs(path), name))
  normalizePath(file.path(p, sprintf("%s.log", package)), mustWork = FALSE)
}

path_logs <- function(path) {
  dir_create(p <- file.path(path, "logs"))
  normalizePath(p)
}

path_sources <- function() {
  dir_create(p <- file.path(tempdir(), "checked_tmp"))
  normalizePath(p)
}

path_check_output <- function(path, check) {
  dir_create(p <- file.path(path, "checks"))
  normalizePath(file.path(p, check), mustWork = FALSE)
}

check_path_is_pkg_source <- function(pkg) {
  stopifnot(
    is.character(pkg),
    length(pkg) == 1,
    dir.exists(pkg),
    path_is_pkg(pkg)
  )

  normalizePath(pkg, mustWork = TRUE)
}

path_is_pkg <- function(path) {
  file.exists(file.path(path, "DESCRIPTION"))
}
