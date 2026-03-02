#' Check functions
#'
#' Set of functions to run orchestrated `R CMD check`s and automatically manage
#' the dependencies installation. Each functions prepares the plan based on the
#' supplied package source(s) which includes installing dependencies and running
#' required `R CMD check`s. All the functions are parallelized through sperate
#' processes
#'
#' @param path file path to the package source directory
#' @param n `integer` value indicating maximum number of subprocesses that can
#'   be simultaneously spawned when executing tasks.
#' @param output `character` value specifying path where the output should be
#'   stored.
#' @param lib.loc `character` vector with libraries allowed to be used when
#'   checking packages, defaults to entire [`.libPaths()`].
#' @param repos `character` vector of repositories which will be used when
#'   generating task graph and later pulling dependencies.
#' @param reverse_repos `character` vector of repositories which will be used
#'   to pull sources for reverse dependencies. In some cases, for instance using
#'   binaries on Linux, we want to use different repositories when pulling
#'   sources to check and different when installing dependencies.
#' @param restore `logical` indicating whether output directory should be
#'   unlinked before running checks. If `FALSE`, an attempt will me made to
#'   restore previous progress from the same `output`
#' @param ... Additional arguments passed to [`run()`]
#'
#' @return
#'   [`checker()`] R6 class storing all the details
#'   regarding checks that run. Can be combined with
#'   [`results`] and [`summary()`] methods to generate results.
#'
#' @keywords internal
#' @name check_functions
NULL

#' Check reverse dependencies
#'
#' Check a package's reverse dependencies in order to identify differences
#' in reverse dependency check results when run alongside your package's
#' development and release versions.
#'
#' Runs classical reverse dependency checks for the given source package. It
#' first identifies reverse dependencies available in `repos`. Then, after
#' installing all required dependencies, runs `R CMD check` twice for each
#' package, one time with the release version of the given source package
#' installed from `repos` and a second time with the development version
#' installed from local source. Both `R CMD checks` are later compared to
#' identify changes in reverse dependency behaviors.
#'
#' @inheritParams check_functions
#' @inheritParams run
#'
#' @inherit check_functions return
#'
#' @family checks
#' @export
check_rev_deps <- function(
  path,
  n = 2L,
  output = tempfile(paste(utils::packageName(), Sys.Date(), sep = "-")),
  lib.loc = .libPaths(),
  repos = getOption("repos"),
  reverse_repos = repos,
  restore = TRUE,
  ...
) {
  checks <- checker$new(
    plan_rev_dep_checks(path = path, repos = reverse_repos),
    n = n,
    output = output,
    lib.loc = lib.loc,
    repos = repos,
    restore = restore
  )

  run(checks, ...)
  checks
}

#' Check packages
#'
#' Runs classical `R CMD check` for the given source package. It
#' first identifies and installs, in parallel, all dependencies required
#' to check the package. Then, it runs `R CMD check` for each specified package.
#'
#' @inheritParams check_functions
#' @inheritParams run
#' @inheritParams plan_local_checks
#'
#' @inherit check_functions return
#'
#' @family checks
#' @export
check_pkgs <- function(
  package,
  n = 2L,
  output = tempfile(paste(utils::packageName(), Sys.Date(), sep = "-")),
  lib.loc = .libPaths(),
  repos = getOption("repos"),
  restore = TRUE,
  ...
) {
  checks <- checker$new(
    plan_local_checks(package = package, repos = repos),
    n = n,
    output = output,
    lib.loc = lib.loc,
    repos = repos,
    restore = restore
  )

  run(checks, ...)
  checks
}
