#' @import cli
"_PACKAGE"

base_pkgs <- function() {
  c("R", utils::installed.packages(priority = "base")[, "Package"])
}

.callr <- as.list(getNamespace("callr"), all.names = TRUE)[c(
  "default_load_hook"
)]

.tools <- as.list(getNamespace("tools"), all.names = TRUE)[c(
  ".split_dependencies"
)]

replace_with_map <- function(x, value, replacement) {
  m <- match(x, value)
  x[which(!is.na(m))] <- replacement[m[!is.na(m)]]
  x
}

hash_alias <- function(x) {
  paste0(c("hash", as.character(charToRaw(x))), collapse = "")
}

dir_create <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, showWarnings = FALSE, recursive = TRUE)
  }
}

`%||%` <- function(x, y) {
  if (!is.null(x)) {
    x
  } else {
    y
  }
}

drlapply <- function(...) {
  do.call(rbind, lapply(...))
}

drmapply <- function(...) {
  do.call(rbind, mapply(..., USE.NAMES = FALSE, SIMPLIFY = FALSE))
}

uulist <- function(...) unique(as.character(unlist(...)))
`%||%` <- function(lhs, rhs) if (is.null(lhs)) rhs else lhs
vcapply <- function(...) vapply(..., FUN.VALUE = character(1L))
vlapply <- function(...) vapply(..., FUN.VALUE = logical(1L))
viapply <- function(...) vapply(..., FUN.VALUE = integer(1L))
vnapply <- function(...) vapply(..., FUN.VALUE = numeric(1L))

is_install_failure_warning <- function(w) {
  patterns <- c(
    "download of package .* failed",
    "(dependenc|package).*(is|are) not available",
    "installation of package.*had non-zero exit status",
    "installation of one or more packages failed"
  )

  re <- paste0("(", paste0(patterns, collapse = "|"), ")")
  grepl(re, w$message)
}
