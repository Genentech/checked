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
  name <- names(node) %||% node # nolint (used via non-standard-evaluation)
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

task_graph_libpaths <- function(g, node = NULL, lib.loc = .libPaths()) {
  vs <- if (is.null(node)) {
    igraph::V(g)
  } else {
    task_graph_neighborhoods(g, node)[[1]]
  }

  # iterate over tasks and derive a library location
  task_lib <- lapply(
    vs$task,
    function(x, ...) lib(x, ...), lib.loc = lib.loc
  )

  unique(unlist(task_lib))
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
  install_parameters <- install_params(task$origin)
  libpaths <- task_graph_libpaths(g, node, lib.loc = lib.loc)

  if (inherits(task$origin, "pkg_origin_base")) {
    return(NULL)
  }

  if (is_package_installed(install_parameters$package, libpaths)) {
    return(NULL)
  }

  install_process$new(
    install_parameters$package,
    lib = path_lib(output),
    libpaths = libpaths,
    repos = task$origin$repos,
    dependencies = FALSE,
    type = task$type,
    INSTALL_opts = c(), # TODO
    log = path_package_install_log(output, friendly_name(task)),
    env = c() # TODO
  )
}

#' @export
start_task.custom_install_task <- function(
  node,
  g,
  output,
  lib.loc,
  ...
) {
  spec <- task_graph_task(g, task)
  install_parameters <- install_params(spec$package)
  libpaths <- c(task_get_lib_loc(g, node, output), lib.loc)
  install_process$new(
    install_parameters$package,
    lib = path_custom_lib(output, spec$alias),
    libpaths = libpaths,
    repos = install_parameters$repos,
    dependencies = FALSE,
    type = spec$type,
    INSTALL_opts = spec$INSTALL_opts,
    log = path_package_install_log(output, spec$alias),
    env = spec$env
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
  libpaths <- task_graph_libpaths(g, node, lib.loc = lib.loc)
  path <- check_path(task$origin, output = path_sources())

  # TODO: make output directory names more user-friendly, for now the vertex
  # hash id is used to disambiguate output
  output_dirname <- paste0(friendly_name(task), " <", node$name[[1]], ">")

  check_process$new(
    path = path,
    check_dir = path_check_output(output, output_dirname),
    libpath = libpaths,
    repos = task$repos,
    args = task$args,
    build_args = task$build_args,
    env = task$env
  )
}
