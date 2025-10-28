#' @import cli
NULL

as_vertex_name <- function(x, ...) {
  UseMethod("as_vertex_name")
}

#' @export
as_vertex_name.default <- function(x, ...) {
  hash(x, ...)
}

#' @export
as_vertex_name.task <- function(x, ...) {
  paste0(
    hash(x, ...),
    gsub("\\s+", "-", fmt(task = x, "-{action}-{package}", ansi = FALSE))
  )
}

#' @export
as_vertex_name.local_check_meta_task <- function(x, ...) {
  paste0(
    hash(x, ...),
    gsub("\\s+", "-", fmt(task = x, "-{action}", ansi = FALSE))
  )
}

hash <- function(x, n = 12) {
  substring(cli::hash_obj_sha256(x), 1, n)
}

hashes <- function(x, ...) {
  vcapply(x, hash, ...)
}

base_pkgs <- function() {
  c("R", utils::installed.packages(priority = "base")[, "Package"])
}

is_package_installed <- function(pkg, lib.loc = .libPaths()) {
  path <- find.package(pkg, lib.loc = lib.loc, quiet = TRUE)
  length(path) > 0
}

.callr <- as.list(getNamespace("callr"), all.names = TRUE)[c(
  "default_load_hook"
)]

.tools <- as.list(getNamespace("tools"), all.names = TRUE)[c(
  ".split_dependencies"
)]

dir_create <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, showWarnings = FALSE, recursive = TRUE)
  }
}

`%||%` <- function(lhs, rhs) if (is.null(lhs)) rhs else lhs

vcapply <- function(...) vapply(..., FUN.VALUE = character(1L))
vlapply <- function(...) vapply(..., FUN.VALUE = logical(1L))
viapply <- function(...) vapply(..., FUN.VALUE = integer(1L))
vnapply <- function(...) vapply(..., FUN.VALUE = numeric(1L))

suppressWarningsRegex <- function(expr, regex, ...) {
  withCallingHandlers(
    expr,
    warning = function(w) {
      if (grepl(pattern = regex, x = conditionMessage(w), ...)) {
        invokeRestart("muffleWarning")
      }
    }
  )
}
