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
  })

  # iterate over tasks and derive a library location
  task_lib <- lapply(vs$task, lib, lib.loc = lib.loc, lib.root = path_libs(output))
  unique(unlist(task_lib))
}

start_task <- function(node, g, ...) {
  UseMethod("start_task")
}

#' @export
start_task.igraph.vs <- function(node, g, ...) {
  stopifnot(length(node) == 1L)
  UseMethod("start_task", node$task[[1]])
}

#' @export
start_task.install_task <- function(
  node,
  g,
  output,
  lib.loc,
  ...
) {
  task <- node$task[[1]]
  #if (length(task$origin$source) > 0 && task$origin$source == "/Users/maksymis/Desktop/validation/code/DALEX") browser()
  libpaths <- task_graph_libpaths(g, node, lib.loc = lib.loc, output = output)
  install_parameters <- install_params(task$origin)

  if (any(inherits(task$origin, c("pkg_origin_base", "pkg_origin_unknown")))) {
    return(NULL)
  }

  if (is_package_installed(install_parameters$package, libpaths)) {
    return(NULL)
  }

  install_process$new(
    install_parameters$package,
    lib = lib(task$lib, lib.loc = lib.loc, lib.root = path_libs(output)),
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
start_task.check_task <- function(
  node,
  g,
  output,
  lib.loc,
  ...
) {
  task <- node$task[[1]]
  libpaths <- task_graph_libpaths(g, node, lib.loc = lib.loc, output = output)
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
