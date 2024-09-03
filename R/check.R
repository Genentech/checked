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
#' @param ... Additional arguments passed to [`checked-task-df`] and [`run()`]
#'
#' @return
#'   [`check_design()`] R6 class storing all the details
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
#' @inheritParams options_params
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
    lib.loc = .libPaths(), # nolint object_name_linter
    repos = getOption("repos"),
    reverse_repos = repos,
    restore = options::opt("restore"),
    reporter = reporter_default(),
    ...) {
  
  checks <- rev_dep_check_tasks_df(
    path = path,
    repos = reverse_repos,
    ...
  )

  plan <- check_design$new(
    checks,
    n = n,
    output = output,
    lib.loc = lib.loc,
    repos = repos,
  )

  run(plan, ...)
  plan
}

#' Run reverse dependency checks against a development version only
#'
#' [`check_dev_rev_deps()`] works similarly to [`check_rev_deps()`] but it runs
#' R CMD check only once for each package, with the development version of the
#' package installed. It is advantageous to check whether adding a new package
#' into a repository breaks existing packages that possibly take said package
#' as a `Suggests` dependency.
#'
#' @inheritParams check_functions
#' @inheritParams options_params
#' @inheritParams run
#'
#' @inherit check_functions return
#'
#' @family checks
#' @export
check_dev_rev_deps <- function(
    path,
    n = 2L,
    output = tempfile(paste(utils::packageName(), Sys.Date(), sep = "-")),
    lib.loc = .libPaths(), # nolint object_name_linter
    repos = getOption("repos"),
    restore = options::opt("restore"),
    ...) {
  
  checks <- rev_dep_check_tasks_df(
    path = path,
    repos = repos,
    versions = "dev",
    ...
  )

  plan <- check_design$new(
    checks,
    n = n,
    output = output,
    lib.loc = lib.loc,
    repos = repos,
    restore = restore
  )

  run(plan, ...)
  plan
}

#' Check one or more package source directories
#'
#' [`check_pkgs()`] Installs all dependencies and runs `R CMD check`s
#' in parallel for all source packages whose source code is found in the
#' `path` directory
#'
#' @inheritParams check_functions
#' @inheritParams options_params
#' @inheritParams run
#'
#' @inherit check_functions return
#'
#' @family checks
#' @export
check_pkgs <- function(
    path,
    n = 2L,
    output = tempfile(paste(utils::packageName(), Sys.Date(), sep = "-")),
    lib.loc = .libPaths(), # nolint object_name_linter
    repos = getOption("repos"),
    restore = options::opt("restore"),
    ...) {
  
  checks <- source_check_tasks_df(path, ...)

  plan <- check_design$new(
    checks,
    n = n,
    output = output,
    lib.loc = lib.loc,
    repos = repos,
    restore = restore
  )

  run(plan, ...)
  plan
}

#' Check all package source directories in current directory
#'
#' [`check_dir()`] Identifies all R packages in the given directory
#' (non-recursively) and passes them to the [`check_pkgs()`]
#'
#' @inheritParams check_functions
#' @inheritParams options_params
#' @inheritParams run
#'
#' @inherit check_functions return
#'
#' @family checks
#' @export
check_dir <- function(
    path,
    n = 2L,
    output = tempfile(paste(utils::packageName(), Sys.Date(), sep = "-")),
    lib.loc = .libPaths(), # nolint object_name_linter
    repos = getOption("repos"),
    restore = options::opt("restore"),
    ...) {
  dirs <- list.dirs(path, full.names = TRUE, recursive = FALSE)
  r_packages <- dirs[vlapply(dirs, path_is_pkg)]

  check_pkgs(
    r_packages,
    n = n,
    output = output,
    lib.loc = lib.loc,
    repos = repos,
    restore = restore,
    ...
  )
}
