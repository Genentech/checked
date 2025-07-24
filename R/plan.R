#' Plan Reverse Dependency Checks
#'
#' Generates a plan for running reverse dependency check for certain
#' source package. In such case `path` should be proivded with a directory
#' path to the development version of the package and `repos` should be a
#' repository for which reverse dependencies should be identified.
#'
#' @param path path to the package source.
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
    return(igraph::make_empty_graph())
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
    return(igraph::make_empty_graph())
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

#' Plan R CMD Checks
#'
#' Generates a plan for running R CMD check for a specified set of packages.
#'
#' @param pacakge A path to either package, directory with packages or name
#'  of the package (details)
#' @param repos repository used to identify packages when name is provided.
#'
#' @details
#' `package` parameter has two different allowed values:
#'  * Package - checked looks for a DESCRIPTION file in the provided path, if
#'    found treats it like a source package.
#'  * If the specified value does not correspond to a source package, the
#'    parameter is treated as the name and `repos` parameter is used to identify
#'    the source.
#'
#'
#' @family plan
#' @export
plan_local_checks <- function(
  path,
  repos = getOption("repos")
) {

  task <- meta_task(
    origin = NULL,
    .subclass = "local_check"
  )

  # build individual plans for each package value
  local_checks_tasks <- lapply(
    path,
    function(x, repos) {
      path <- try(check_path_is_pkg_source(x), silent = TRUE)
      if (inherits(path, "try-error")) {
        check_task(
          origin = pkg_origin_repo(package = x, repos = repos),
          env = options::opt("check_envvars"),
          args = options::opt("check_args"),
          build_args = options::opt("check_build_args")
        )
      } else {
        check_task(
          origin = pkg_origin_local(x),
          env = options::opt("check_envvars"),
          args = options::opt("check_args"),
          build_args = options::opt("check_build_args")
        )
      }
    },
    repos = repos
  )

  g <- star_graph(
    task = c(
      list(task),
      local_checks_tasks
    )
  )

  E(g)$type <- rep(DEP$Depends, times = length(E(g)))

  g <- task_graph_class(g)

  if (remotes_permitted()) {
    remotes_graph(g)
  } else {
    g
  }
}
