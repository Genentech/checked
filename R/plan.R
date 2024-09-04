empty_checks_df <- data.frame(
  alias = character(0),
  version = character(0),
  package = character(0),
  custom = character(0)
)

#' Check Plan
#'
#' Plans are pre-specified sets of checks. Plans are simple `data.frame`s
#' where each row defines a package for which `R CMD check`
#' should be run.
#'
#' @param path path to the package source. Can be either a single source
#'   code directory or a directory containing multiple package source code
#'   directories.
#'
#' @return The check schedule `data.frame` with the following columns:
#'
#' * `alias`: The alias of the check to run. It also serves the purpose of
#'   providing a unique identifier and node name in the task graph.
#' * `version`: Version of the package to be checked.
#' * `package`: Object that inherits from [`check_task()`].
#'   Defines how package to be checked can be acquired.
#' * `custom`:  Object that inherits from [`custom_install_task()`].
#'   Defines custom package, for instance only available from local source, that
#'   should be installed before checking the package.
#'
#' @family plan
#' @name plan
NULL

#' Plan Reverse Dependency Checks
#'
#' Generates a plan for running reverse dependency check for certain
#' source package. In such case `path` should be proivded with a directory
#' path to the development version of the package and `repos` should be a
#' repository for which reverse dependencies should be identified.
#'
#' @inherit plan
#' @inheritParams plan
#' @param repos repository used to identify reverse dependencies.
#' @param versions character vector indicating against which versions of the
#'   package reverse dependency should be checked. `c("dev", "release")`
#'   (default) stands for the classical reverse dependency check. `"dev"`
#'   checks only against development version of the package which is applicable
#'   mostly when checking whether adding new package would break tests of
#'   packages already in the repository and take the package as suggests
#'   dependency.
#'
#' @family plan
#' @export
plan_rev_dep_checks <- function(
  path,
  repos = getOption("repos"),
  versions = c("dev", "release")
) {
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

  tasks_function <- if (all(c("dev", "release") %in% versions)) {
    rev_dep_check_tasks
  } else {
    rev_dep_check_tasks_development
  }

  if ("dev" %in% versions) {
    df_dev$alias <- paste0(df_dev$alias, " (dev)")
    df_dev$package <- tasks_function(revdeps, repos, df_dev$alias, "new")
    df_dev$custom <- rep(list(custom_install_task(
      alias = paste0(package, " (dev)"),
      package = pkg_origin_local(name = package, path = path),
      type = "source"
    )), times = NROW(df_dev))
  }

  if ("release" %in% versions) {
    package_v <- ap[package, "Version"]
    df_rel$alias <- paste0(df_rel$alias, " (v", package_v, ")")
    df_rel$package <- tasks_function(revdeps, repos, df_rel$alias, "old")
    df_rel$custom <- rep(list(custom_install_task(
      alias = paste0(package, " (release)"),
      package = pkg_origin_repo(name = package, repos = repos),
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

  df$package <- list_of_task(df$package)
  df$custom <- list_of_task(df$custom)
  df
}

rev_dep_check_tasks <- function(packages, repos, aliases, revdep) {
  list_of_task(mapply(
    function(package, alias) {
      revdep_check_task(
        alias = alias,
        package = pkg_origin_repo(name = package, repos = repos),
        env = DEFAULT_R_CMD_CHECK_ENVVARS,
        args = DEFAULT_R_CMD_CHECK_ARGS,
        build_args = DEFAULT_R_CMD_BUILD_ARGS,
        revdep = revdep
      )
    },
    packages,
    aliases,
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE
  ))
}

rev_dep_check_tasks_development <- function(
  packages,
  repos,
  aliases,
  ...
) {
  list_of_task(mapply(
    function(package, alias) {
      check_task(
        alias = alias,
        package = pkg_origin_repo(name = package, repos = repos),
        env = DEFAULT_R_CMD_CHECK_ENVVARS,
        args = DEFAULT_R_CMD_CHECK_ARGS,
        build_args = DEFAULT_R_CMD_BUILD_ARGS
      )
    },
    packages,
    aliases,
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE
  ))
}

#' Plan multiple R CMD checks from source package paths
#'
#' @inherit plan
#' @inheritParams plan
#'
#' @family plan
#' @export
plan_checks <- function(path) {
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

  df <- data.frame(alias = alias, version = version)
  df$package <- list_of_task(source_check_tasks(package, path, alias))
  df$custom <- list_of_task(rep(list(custom_install_task()), times = NROW(df)))

  df
}

source_check_tasks <- function(packages, path, aliases) {
  list_of_task(mapply(
    function(package, path, alias) {
      check_task(
        alias = alias,
        package = pkg_origin_local(name = package, path = path),
        env = DEFAULT_R_CMD_CHECK_ENVVARS,
        args = DEFAULT_R_CMD_CHECK_ARGS,
        build_args = DEFAULT_R_CMD_BUILD_ARGS
      )
    },
    packages,
    path,
    aliases,
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE
  ))
}
