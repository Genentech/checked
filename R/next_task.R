next_task_to_run <- function(g) {
  checks <- task_graph_which_check_satisfied(g)
  installs <- task_graph_which_install_satisfied(g)

  # Prioritize checks overs installs
  v <- igraph::V(g)[c(checks, installs)]
  utils::head(v, 1L)
}

#' @importFrom igraph .env
task_get_lib_loc <- function(g, node, output) {
  nhood <- task_graph_neighborhoods(g, node)[[1]]
  name <- names(node) %||% node
  nhood <- nhood[names(nhood) != .env$name]
  # Custom packages are possible only for the check type nodes which are
  # always terminal. Therefore if we sort nhood making custom packages appear
  # first, their lib will always be prioritized
  attributes <- igraph::vertex.attributes(g, index = nhood)

  paths <- vcapply(nhood, function(v) {
    task_get_install_lib(g, v, output)
  })

  unique(paths[order(attributes$custom, decreasing = TRUE)])
}

task_get_install_lib <- function(g, node, output) {
  attributes <- igraph::vertex.attributes(g, index = node)
  if (attributes$type == "check") {
    path_check_output(output, attributes$spec[[1]]$alias)
  } else if (attributes$custom) {
    path_custom_lib(output, attributes$spec[[1]]$alias)
  } else {
    path_lib(output)
  }
}

start_task <- function(task, g, ...) {
  UseMethod("start_task", task_graph_task_spec(g, task))
}

#' @export
start_task.install_task_spec <- function(task, g, output, lib.loc, ...) {
  spec <- task_graph_task_spec(g, task)
  install_parameters <- install_parameters(spec$package_spec)
  libpaths <- c(task_get_lib_loc(g, task, output), lib.loc)
  install_packages_process$new(
    install_parameters$package,
    lib = path_lib(output),
    libpaths = libpaths,
    repos = install_parameters$repos,
    type = spec$type,
    INSTALL_opts = spec$INSTALL_opts,
    log = path_package_install_log(output, spec$alias),
    env = spec$env
  )
}

#' @export
start_task.custom_install_task_spec <- function(task, g, output, lib.loc, ...) {
  spec <- task_graph_task_spec(g, task)
  install_parameters <- install_parameters(spec$package_spec)
  libpaths <- c(task_get_lib_loc(g, task, output), lib.loc)
  install_packages_process$new(
    install_parameters$package,
    lib = path_custom_lib(output, spec$alias),
    libpaths = libpaths,
    repos = install_parameters$repos,
    type = spec$type,
    INSTALL_opts = spec$INSTALL_opts,
    log = path_package_install_log(output, spec$alias),
    env = spec$env
  )
}

#' @export
start_task.check_task_spec <- function(task, g, output, lib.loc, ...) {
  spec <- task_graph_task_spec(g, task)
  libpaths <- c(task_get_lib_loc(g, task, output), lib.loc)
  path <- check_path(spec$package_spec, output = path_sources())

  check_process$new(
    path = path,
    check_dir = path_check_output(output, spec$alias),
    libpath = libpaths,
    repos = spec$package_spec$repos,
    args = spec$args,
    build_args = spec$build_args,
    env = spec$env
  )
}
