empty_checks_df <- data.frame(
  alias = character(0),
  version = character(0),
  package = character(0),
  custom = character(0)
)

#' Check schedule data frame
#'
#' Create data.frame which each row defines a package for which R CMD check
#' should be run. Such data.frame is a prerequisite for generating
#' [`check_design()`] which orchestrates all the processes
#' including dependencies installation.
#'
#' @details
#'
#' `_tasks_df()` functions generate check task `data.frame` for
#' all source packages specified by the `path`. Therefore it accepts it to be
#' a vector of an arbitrary length.
#'
#' @param path path to the package source. Can be either a single source
#'   code directory or a directory containing multiple package source code
#'   directories.
#' @param ... parameters passed to the task specs allowing to customize
#'   subprocesses.
#'
#' @return The check schedule `data.frame` with the following columns:
#'
#' * `alias`: The alias of the check to run. It also serves the purpose of
#'   providing a unique identifier and node name in the task graph.
#' * `version`: Version of the package to be checked.
#' * `package`: Object that inherits from [`check_task_spec()`].
#'   Defines how package to be checked can be acquired.
#' * `custom`:  Object that inherits from [`custom_install_task_spec()`].
#'   Defines custom package, for instance only available from local source, that
#'   should be installed before checking the package.
#'
#' @family tasks
#' @name checked-task-df
NULL

#' Build Tasks for Reverse Dependency Checks
#"
#' Generates checks schedule data.frame appropriate for running reverse
#' dependency check for certain source package. In such case `path` parameter
#' should point to the source of the development version of the package and
#' `repos` should be a repository for which reverse dependencies should be
#' identified.
#'
#' @inherit checked-task-df
#' @inheritParams checked-task-df
#' @param repos repository used to identify reverse dependencies.
#' @param versions character vector indicating against which versions of the
#'   package reverse dependency should be checked. `c("dev", "release")`
#'   (default) stands for the classical reverse dependency check. `"dev"`
#'   checks only against development version of the package which is applicable
#'   mostly when checking whether adding new package would break tests of
#'   packages already in the repository and take the package as suggests
#'   dependency.
#'
#' @family tasks
#' @export
rev_dep_check_tasks_df <- function(
  path,
  repos = getOption("repos"),
  versions = c("dev", "release"),
  ...
) {
  stopifnot(
    "rev_dep_check_tasks_df requires path argument of length 1" =
      length(path) == 1
  )

  versions <- match.arg(versions, c("dev", "release"), several.ok = TRUE)
  ap <- utils::available.packages(repos = repos)
  path <- check_path_is_pkg_source(path)
  package <- get_package_name(path)
  revdeps <- tools::package_dependencies(
    package,
    which = "all",
    reverse = TRUE,
    db = ap
  )[[1]]

  if (length(revdeps) == 0) {
    return(empty_checks_df)
  }

  version <- ap[revdeps, "Version"]
  df_dev <- df_rel <- data.frame(
    alias = revdeps,
    version = version
  )

  if (!package %in% ap[, "Package"] && "release" %in% versions) {
    warning(
      sprintf(
        "Package `%s` not found in repositories `%s`. Skipping 'release' in 'versions'",
        package,
        paste0(repos, collapse = ", ")
      ),
      immediate. = TRUE
    )
    if ("dev" %in% versions) {
      versions <- "dev"
    } else {
      return(empty_checks_df)
    }
  }

  task_specs_function <- if (all(c("dev", "release") %in% versions)) {
    rev_dep_check_tasks_specs
  } else {
    rev_dep_check_tasks_specs_development
  }

  if ("dev" %in% versions) {
    df_dev$alias <- paste0(df_dev$alias, " (dev)")
    df_dev$package <- task_specs_function(revdeps, repos, df_dev$alias, "new", ...)
    df_dev$custom <- rep(list(custom_install_task_spec(
      alias = paste0(package, " (dev)"),
      package_spec = package_spec_source(name = package, path = path),
      type = "source"
    )), times = NROW(df_dev))
  }

  if ("release" %in% versions) {
    package_v <- ap[package, "Version"]
    df_rel$alias <- paste0(df_rel$alias, " (v", package_v, ")")
    df_rel$package <- task_specs_function(revdeps, repos, df_rel$alias, "old", ...)
    df_rel$custom <- rep(list(custom_install_task_spec(
      alias = paste0(package, " (release)"),
      package_spec = package_spec(name = package, repos = repos),
      # make sure to use the release version built against the same system
      type = "source"
    )), times = NROW(df_dev))
  }

  if (identical(versions, "dev")) {
    df <- df_dev
  } else if (identical(versions, "release")) {
    df <- df_rel
  } else {
    idx <- rep(seq_len(nrow(df_rel)), each = 2) + c(0, nrow(df_rel))
    df <- rbind(df_dev, df_rel)[idx, ]
  }

  df$package <- list_of_task_spec(df$package)
  df$custom <- list_of_task_spec(df$custom)
  df
}

rev_dep_check_tasks_specs <- function(packages, repos, aliases, revdep, ...) {
  list_of_task_spec(mapply(
    function(
      p,
      a,
      env = options::opt("check_envvars"),
      args = options::opt("check_args"),
      build_args = options::opt("check_build_args")
    ) {
      revdep_check_task_spec(
        alias = a,
        package_spec = package_spec(name = p, repos = repos),
        env = env,
        args = args,
        build_args = build_args,
        revdep = revdep
      )
    },
    packages,
    aliases,
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE,
    MoreArgs = list(...)
  ))
}

rev_dep_check_tasks_specs_development <- function(
  packages,
  repos,
  aliases,
  revdep,
  ...
) {
  list_of_task_spec(mapply(
    function(
      p,
      a,
      env = options::opt("check_envvars"),
      args = options::opt("check_args"),
      build_args = options::opt("check_build_args")
    ) {
      check_task_spec(
        alias = a,
        package_spec = package_spec(name = p, repos = repos),
        env = env,
        args = args,
        build_args = build_args
      )
    },
    packages,
    aliases,
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE,
    MoreArgs = list(...)
  ))
}

#' Create a Task to Check a Package from Source
#'
#' @inherit checked-task-df
#' @inheritParams checked-task-df
#'
#' @family tasks
#' @export
source_check_tasks_df <- function(path, ...) {
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
    source_check_tasks_specs(package, path, alias, ...)
  )
  df$custom <- list_of_task_spec(
    rep(list(custom_install_task_spec()), times = NROW(df))
  )

  df
}

source_check_tasks_specs <- function(packages, path, aliases, ...) {
  list_of_task_spec(mapply(
    function(
      p,
      path,
      a,
      env = options::opt("check_envvars"),
      args = options::opt("check_args"),
      build_args = options::opt("check_build_args")) {
      
      check_task_spec(
        alias = a,
        package_spec = package_spec_source(name = p, path = path, repos = NULL),
        env = env,
        args = args,
        build_args = build_args
      )
    },
    packages,
    path,
    aliases,
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE,
    MoreArgs = list(...)
  ))
}
