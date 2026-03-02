#' Libpaths from task graph
#'
#' Function that traverses over the task dependency task to acquire libpaths
#' for given nodes. It ensures that when runing a node, a libpath is
#' constructed which has all the required packages on it.
#'
#' @param g `task_graph` object
#' @param node Node(s) for which libpath should be constructed based on `g`
#' @param output Path to the checked output directory
#' @inheritParams lib
#'
#' @keywords internal
task_graph_libpaths <- function(
  g,
  node = NULL,
  lib.loc = .libPaths(),
  output = tempdir()
) {
  # Maintain original vertices order to make sure libpaths are properly
  # constructed
  vs <- sort(if (is.null(node)) {
    igraph::V(g)
  } else {
    task_graph_neighborhoods(g, node)[[1]]
  }, decreasing = TRUE)

  # iterate over tasks and derive a library location
  task_lib <- lapply(
    vs$task,
    lib,
    lib.loc = lib.loc,
    lib.root = path_libs(output)
  )
  unique(unlist(task_lib))
}

#' Start a new task
#'
#' Starts task based on the `task` object encapsulated in the `node` taken
#' from then `task_graph` `g`. It returns an `install_process` or
#' `check_process` `R6` object.
#'
#' @inheritParams task_graph_libpaths
#' @param ... additional params passed to downstream methods
#'
#' @keywords internal
start_task <- function(node, g, ...) {
  UseMethod("start_task")
}

#' @export
#' @method start_task igraph.vs
start_task.igraph.vs <- function(node, g, ...) {
  stopifnot(length(node) == 1L)
  UseMethod("start_task", node$task[[1]])
}

#' @export
#' @method start_task install_task
start_task.install_task <- function(
  node,
  g,
  output,
  lib.loc,
  ...
) {
  task <- node$task[[1]]
  libpaths <- unique(c(
    task_graph_libpaths(g, node, lib.loc = lib.loc, output = output),
    lib.loc
  ))
  install_parameters <- install_params(task$origin)

  if (any(inherits(task$origin, c("pkg_origin_base", "pkg_origin_unknown")))) {
    return(NULL)
  }

  # install_parameters$package is a valid package name only for
  # pkg_origin_repo. Otherwise it's a path to the source package in which case
  # is_package_installed returns FALSE (as it should)
  if (is_package_installed(install_parameters$package, libpaths)) {
    return(NULL)
  }

  install_process$new(
    install_parameters$package,
    lib = lib(task, lib.loc = lib.loc, lib.root = path_libs(output)),
    libpaths = libpaths,
    repos = task$origin$repos,
    dependencies = FALSE,
    type = task$type,
    INSTALL_opts = c(), # TODO
    log = path_install_log(output, node$name[[1]]),
    env = c() # TODO
  )
}

#' @export
#' @method start_task check_task
start_task.check_task <- function(
  node,
  g,
  output,
  lib.loc,
  ...
) {
  task <- node$task[[1]]
  libpaths <- unique(c(
    task_graph_libpaths(g, node, lib.loc = lib.loc, output = output),
    lib.loc
  ))
  path <- check_path(task$origin, output = path_sources())

  check_process$new(
    path = path,
    check_dir = path_check_output(output, node$name[[1]]),
    libpath = libpaths,
    repos = task$repos,
    args = task$args,
    build_args = task$build_args,
    env = task$env
  )
}
