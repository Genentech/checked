#' Check functions
#' 
#' Set of functions to run orchestrated R CMD checks and automatically manage
#' the dependencies installation. Each functions prepares the plan based on the
#' supplied package source(s) which includes installing dependencies and running
#' required R CMD checks. All the functions are parallelized through sperate processes
#' 
#' @param path path to the package source.
#' @param n integer value indicating maximum number of subprocesses that can
#' be simultaneously spawned when executing tasks.
#' @param output character value specifying path where the output should be stored.
#' @param lib.loc character vector with libraries allowed to be used when
#' checking packages, defaults to entire .libPaths().
#' @param repos character vector of repositories which will be used when
#' generating task graph and later pulling dependencies.
#' @param reverse_repos character vector of repositories which will be used
#' to pull sources for reverse dependencies. In some cases, for instance using
#' binaries on Linux, we want to use different repositories when pulling sources
#' to check and different when installing dependencies.
#' @param restore logical value, whether output directory should be unlinked
#' before running checks. If FALSE, an attempt will me made to restore previous
#' progress from the same \code{output}
#' @param ... other parameters
#' @inheritParams run
#' 
#' @details
#' 
#' \code{check_reverse_dependencies} runs classical reverse dependency check for
#' the given source package. It first identifies reverse dependencies available 
#' in repos. Then, after installing all required dependencies, it runs the R CMD check 
#' twice for each package, one time with the release version of the package and
#' the second time with the development version. Both R CMD checks are later compared
#' to get the result.
#' 
#' \code{check_reverse_dependencies_development} works similarly to \code{check_reverse_dependencies} 
#' but it runs R CMD check only once for each package, with the development version of
#' the package installed. It is advantageous to check whether adding a new
#' package into repository breaks existing packages that possibly take said package
#' as Suggests dependency.
#' 
#' \code{check_packages} Installs all dependencies and runs parallelly R CMD checks 
#' for all source packages specified by \code{path} parameter
#' 
#' \code{check_dir} Identifies all R packages in the given directory (non-recursively)
#' and passes them to the \code{check_packages}
#' 
#' @return 
#' \code{\link[checked]{check_design}} R6 class storing all the details
#' regarding checks that run. Can be combined with \code{\link[checked]{results}}
#' and \code{summary} methods to generate results.
#' 
#' @name check_functions
NULL

#' @rdname check_functions
#' @export
check_reverse_dependencies <- function(
    path,
    n = 2L,
    output = tempfile(paste(utils::packageName(), Sys.Date(), sep = "-")),
    lib.loc = .libPaths(),
    repos = getOption("repos"),
    reverse_repos = repos,
    restore = TRUE,
    reporter = default_reporter(),
    ...
) {
  checks <- rev_dep_check_tasks_df(path = path, repos = reverse_repos)
  
  plan <- check_design$new(
    checks,
    n = n,
    output = output,
    lib.loc = lib.loc,
    repos = repos,
    restore = restore
  )
  
  run(plan, reporter = reporter)
  plan
}

#' @rdname check_functions
#' @export
check_reverse_dependencies_development <- function(
    path,
    n = 2L,
    output = tempfile(paste(utils::packageName(), Sys.Date(), sep = "-")),
    lib.loc = .libPaths(),
    repos = getOption("repos"),
    restore = TRUE,
    reporter = default_reporter(),
    ...
) {
  checks <- rev_dep_check_tasks_df(path = path, repos = repos, development_only = TRUE)
  
  plan <- check_design$new(
    checks,
    n = n,
    output = output,
    lib.loc = lib.loc,
    repos = repos,
    restore = restore
  )
  
  run(plan, reporter = reporter)
  plan
}

#' @rdname check_functions
#' @export
check_packages <- function(
    path,
    n = 2L,
    output = tempfile(paste(utils::packageName(), Sys.Date(), sep = "-")),
    lib.loc = .libPaths(),
    repos = getOption("repos"),
    restore = TRUE,
    reporter = default_reporter(),
    ...
) {
  checks <- source_check_tasks_df(path)
  
  plan <- check_design$new(
    checks,
    n = n,
    output = output,
    lib.loc = lib.loc,
    repos = repos,
    restore = restore
  )
  
  run(plan, reporter = reporter)
  plan
}

#' @rdname check_functions
#' @export
check_dir <- function(
    path,
    n = 2L,
    output = tempfile(paste(utils::packageName(), Sys.Date(), sep = "-")),
    lib.loc = .libPaths(),
    repos = getOption("repos"),
    restore = TRUE,
    reporter = default_reporter(),
    ...
) {
  dirs <- list.dirs(path, full.names = TRUE, recursive = FALSE)
  r_packages <- dirs[vlapply(dirs, is_package)]
  
  check_packages(
    r_packages,
    n = n,
    output = output,
    lib.loc = lib.loc,
    repos = repos,
    restore = restore,
    reporter = reporter,
    ...
  )
  
}
