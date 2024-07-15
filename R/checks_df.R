#' Check schedule data frame
#'
#' Create data.frame which each row defines a package for which R CMD check 
#' should be run. Such data.frame is a prerequisite for generating 
#' \code{\link[checked]{check_design}} which orchestrates all the processes
#' including dependencies installation.
#' 
#' @param path path to the package source. See Details.
#' @param repos repository used to identify reverse dependencies.
#' @param development_only logical whether reverse dependency check should be run
#' only against development version of the package. Applicable mostly when checking
#' whether adding new package would break tests of packages already in the
#' repository and taking the package as suggests dependency. Default to FALSE.
#' 
#' @details
#' 
#' \code{rev_dep_check_tasks_df} generates checks schedule data.frame appropriate
#' for running reverse dependency check for certain source package. In such case
#' \code{path} parameter should point to the source of the development version of 
#' the package and \code{repos} should be a repository for which reverse 
#' dependencies should be identified.
#' 
#' \code{source_check_tasks_df} generates checks schedule data.frame for all source
#' packages specified by the \code{path}. Therefore it accepts it to be a vector
#' of an arbitrary length.
#' 
#' @return The check schedule data.frame has strict structure and consists of following columns:
#' 
#' \itemize{
#' \item \code{alias} The alias of the check to run. It also serves the purpose of u
#' unique identifier and node name in the task graph.
#' \item \code{version} Version of the package to be checked.
#' \item \code{package} Object that inherits from \code{\link[checked]{check_task_spec}}.
#' Defines how package to be checked can be acquired.
#' \item \code{custom}  Object that inherits from \code{\link[checked]{custom_install_task_spec}}. 
#' Defines custom package, for instance only available from local source, that 
#' should be installed before checking the package.
#' }
#' 
#' @name checks_df
NULL

#' @export
#' @rdname checks_df
rev_dep_check_tasks_df <- function(path, repos = getOption("repos"), development_only = FALSE) {
  stopifnot(
    "rev_dep_check_tasks_df requires path argument of length 1" = length(path) == 1
    )
  ap <- utils::available.packages(repos = repos)
  path <- check_path_is_pkg_source(path)
  package <- get_package_name(path)
  package_v <- ap[package, "Version"]
  revdeps <- tools::package_dependencies(package, which = "all", reverse = TRUE, db = ap)[[1]]
  version <- ap[revdeps, "Version"]
  df_dev <- df_rel <- data.frame(
    alias = revdeps,
    version = version
  )
  
  task_specs_function <- if (development_only) {
    rev_dep_check_tasks_specs_development
  } else {
    rev_dep_check_tasks_specs
  }

  df_dev$alias <- paste0(df_dev$alias, " (dev)")
  df_dev$package <- task_specs_function(revdeps, repos, df_dev$alias, "new")
  df_dev$custom <- rep(list(custom_install_task_spec(
    alias = paste0(package, " (dev)"),
    package = package_spec_source(name = package, path = path),
    type = "source"
  )), times = NROW(df_dev))
  
  if (development_only) {
    df_dev$package <- list_of_task_spec(df_dev$package)
    df_dev$custom <- list_of_task_spec(df_dev$custom)
    return(df_dev)
  }
  
  df_rel$alias <- paste0(df_rel$alias, " (v", package_v, ")")
  df_rel$package <- task_specs_function(revdeps, repos, df_rel$alias, "old")
  df_rel$custom <- rep(list(custom_install_task_spec()), times = NROW(df_dev))
  
  idx <- rep(seq_len(nrow(df_rel)), each = 2) + c(0, nrow(df_rel))
  df <- rbind(df_dev, df_rel)[idx, ]
  
  df$package <- list_of_task_spec(df$package)
  df$custom <- list_of_task_spec(df$custom)
  df
}

rev_dep_check_tasks_specs <- function(packages, repos, aliases, revdep) {
  list_of_task_spec(mapply(
    function(p, a) {
      revdep_check_task_spec(
        alias = a,
        package_spec = package_spec(name = p, repos = repos),
        env = DEFAULT_R_CMD_CHECK_VARIABLES,
        args = DEFAULT_CHECK_ARGS,
        build_args = DEFAULT_BUILD_ARGS,
        revdep = revdep
      )
    },
    packages,
    aliases,
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE
  ))
}

rev_dep_check_tasks_specs_development <- function(packages, repos, aliases, ...) {
  list_of_task_spec(mapply(
    function(p, a) {
      check_task_spec(
        alias = a,
        package_spec = package_spec(name = p, repos = repos),
        env = DEFAULT_R_CMD_CHECK_VARIABLES,
        args = DEFAULT_CHECK_ARGS,
        build_args = DEFAULT_BUILD_ARGS
      )
    },
    packages,
    aliases,
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE
  ))
}


#' @export
#' @rdname checks_df
source_check_tasks_df <- function(path) {
  name <- names(path)
  path <- vcapply(path, check_path_is_pkg_source, USE.NAMES = FALSE)
  package <- vcapply(path, get_package_name)
  alias <- if (is.null(name)) {
    unlist(lapply(unique(package), function(p) {
      idx <- package == p
      suffixes <- if (sum(idx) > 1) {
         paste0("_", seq(sum(idx)))
      } else {
        ""
      }
      paste0(p, " (source", suffixes, ")")
    }))
  } else {
    name
  }
  version <- vcapply(path, get_package_version)
  
  df <- data.frame(
    alias = alias,
    version = version
  )
  
  df$package <- list_of_task_spec(
    source_check_tasks_specs(package, path, alias)
  )
  df$custom <- list_of_task_spec(
    rep(list(custom_install_task_spec()), times = NROW(df))
  )
  
  df
}


source_check_tasks_specs <- function(packages, path, aliases) {
  list_of_task_spec(mapply(
    function(p, path, a) {
      check_task_spec(
        alias = a,
        package_spec = package_spec_source(name = p, path = path, repos = NULL),
        env = DEFAULT_R_CMD_CHECK_VARIABLES,
        args = DEFAULT_CHECK_ARGS,
        build_args = DEFAULT_BUILD_ARGS
      )
    },
    packages,
    path,
    aliases,
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE
  ))
}
