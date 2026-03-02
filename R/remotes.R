remotes_graph <- function(x, ...) {
  UseMethod("remotes_graph")
}

#' @export
remotes_graph.task_graph <- function(x, ...) {
  vs <- V(x)
  remotes_subgraphs <- lapply(vs, remotes_graph, vs = vs)

  task_graph_class(
    suppressWarningsRegex(
      graph_dedup_attrs(
        igraph::union(
          x,
          do.call(igraph::union, remotes_subgraphs)
        )
      ),
      "Some, but not all graphs are named, not using vertex names",
      fixed = TRUE
    )
  )
}

#' @export
remotes_graph.integer <- function(x, ..., vs) {
  remotes_graph(vs[[x]])
}

#' @export
#' @method remotes_graph igraph.vs
remotes_graph.igraph.vs <- function(x, ...) {
  remotes_graph(x$task)
}

#' @export
remotes_graph.task <- function(x, ...) {
  # By default does not allow remotes for a task unless an explicit method
  # has been defined for that type of task
  igraph::make_empty_graph()
}

#' @export
remotes_graph.install_task <- function(x, ...) {
  remote_tasks <- get_remote_tasks(x)
  if (length(remote_tasks) == 0) return(igraph::make_empty_graph())
  remote_tasks_names <- vcapply(remote_tasks, package)

  x_deps <- pkg_deps(x$origin)
  x_remote_deps <- x_deps[
    x_deps$package == package(x) & x_deps$name %in% remote_tasks_names,
  ]

  # Sort tasks according to same key
  remote_tasks <- remote_tasks[order(remote_tasks_names)]
  remote_tasks_types <- x_remote_deps[order(x_remote_deps$name), ]$type

  g <- star_graph(
    task = c(
      list(x),
      remote_tasks
    ),
    edge_attrs = list(type = remote_tasks_types)
  )

  # Recursively get remote_tasks of remote_tasks
  remote_subgraphs <- lapply(remote_tasks, remotes_graph)
  suppressWarningsRegex(
    graph_dedup_attrs(
      igraph::union(
        g,
        do.call(igraph::union, remote_subgraphs)
      )
    ),
    "Some, but not all graphs are named, not using vertex names",
    fixed = TRUE
  )
}

#' @export
remotes_graph.check_task <- remotes_graph.install_task

get_remote_tasks <- function(x) {
  UseMethod("get_remote_tasks")
}

#' @export
get_remote_tasks.default <- function(x) {
  NULL
}

#' @export
get_remote_tasks.task <- function(x) {
  get_remote_tasks(x$origin)
}

#' @export
get_remote_tasks.pkg_origin_local <- function(x) {
  pkgs <- .remotes()$extra_deps(as.package.remotes(x$source), "remotes")

  lapply(seq_len(NROW(pkgs)), function(i) {
    install_task(
      pkg_origin_remote(remote = pkgs$remote[[i]])
    )
  })
}

#' @export
get_remote_tasks.pkg_origin_remote <- function(x) {
  x <- sanitize_pkg_origin_remote(x)
  NextMethod()
}

get_remote_package_source <- function(remote) {
  path <- file.path(path_remotes(), hash(remote))
  if (dir.exists(path)) return(path)
  dir_create(path)
  tmp <- remotes::remote_download(remote, quiet = TRUE)
  file.copy(
    from = list.files(tmp, full.names = TRUE, recursive = FALSE),
    to = path,
    recursive = TRUE
  )
  path
}

remotes_permitted <- function() {
  options::opt("add_remotes") && requireNamespace("remotes", quietly = TRUE)
}
