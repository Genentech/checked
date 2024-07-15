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
  valid_name <- raw_based_hash(custom)
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
