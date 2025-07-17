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
  version_types <- match.arg(versions, c("dev", "release"), several.ok = TRUE)
  path <- check_path_is_pkg_source(path)
  ap <- available_packages(repos = repos)

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

  if ("release" %in% version_types && !package %in% ap[, "Package"]) {
    msg <- sprintf(
      "Skipping 'release' checks. Package `%s` not found in repositories: \n",
      package,
      paste0(" * ", repos, collapse = "\n")
    )

    warning(msg, immediate. = TRUE)
    version_types <- setdiff(version_types, "release")
  }

  if (length(version_types) == 0) {
    return(empty_checks_df)
  }

  # root meta task, indicating a reverse-dependency check plan
  task <- sequence_graph(task = list(meta_task(
    origin = pkg_origin_local(path),
    .subclass = "rev_dep_dep"
  )))

  # build individual plans for development version reverse-dependency checks
  rev_dep_dev_check_tasks <- lapply(
    revdeps,
    plan_rev_dep_dev_check,
    origin = pkg_origin_local(path),
    repos = repos
  )

  # build individual plans for release version reverse-dependency checks
  rev_dep_release_check_tasks <- lapply(
    revdeps[revdeps %in% ap[, "Package"]],
    plan_rev_dep_release_check,
    origin = pkg_origin_local(path),
    repos = repos
  )

  # store vertex names so that we can build edges after merging
  task_id <- V(task)[[1]]$name
  rev_dep_meta_task_ids <- unique(c(
    vcapply(rev_dep_dev_check_tasks, function(g) V(g)[[1]]$name),
    vcapply(rev_dep_release_check_tasks, function(g) V(g)[[1]]$name)
  ))

  # combine component plans into an overall plan
  g <- graph_dedup_attrs(igraph::union(
    task,
    rev_dep_dev_check_tasks,
    rev_dep_release_check_tasks
  ))

  # reconstruct edges from planned root node to individual checks
  edges <- as.vector(rbind(task_id, rev_dep_meta_task_ids))

  # meta tasks take dependency relationship with subtasks
  g <- igraph::add_edges(g, edges = edges, attr = list(type = DEP$Depends))
  E(g)$type <- DEP[E(g)$type]

  g <- task_graph_class(g)

  if (remotes_permitted()) {
    remotes_graph(g)
  } else {
    g
  }
}

plan_rev_dep_dev_check <- function(origin, revdep, repos) {
  rev_dep_origin <- pkg_origin_repo(package = revdep, repos = repos)
  sequence_graph(task = list(
    meta_task(
      origin = origin,
      revdep = revdep,
      .subclass = "rev_dep_check"
    ),
    make_unique_task(seed = "dev", check_task(
      origin = rev_dep_origin,
      env = options::opt("check_envvars"),
      args = options::opt("check_args"),
      build_args = options::opt("check_build_args")
    )),
    install_task(origin = origin)
  ))
}

plan_rev_dep_release_check <- function(origin, revdep, repos) {
  rev_dep_origin <- pkg_origin_repo(package = revdep, repos = repos)
  repo_root_origin <- try_pkg_origin_repo(
    package = package(origin),
    repos = repos
  )
  sequence_graph(task = list(
    meta_task(
      origin = origin,
      revdep = revdep,
      .subclass = "rev_dep_check"
    ),
    make_unique_task(seed = "release", check_task(
      origin = rev_dep_origin,
      env = options::opt("check_envvars"),
      args = options::opt("check_args"),
      build_args = options::opt("check_build_args")
    )),
    install_task(
      origin = repo_root_origin,
      lib = lib_path_isolated(origin = repo_root_origin)
    )
  ))
}
