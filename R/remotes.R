remotes_graph <- function(x, ...) {
  UseMethod("remotes_graph")
}

#' @export
remotes_graph.task_graph <- function(x, ...) {
  vs <- V(x)
  remotes_subgraphs <- lapply(vs, remotes_graph, vs = vs)
  
  graph_dedup_attrs(igraph::union(x, remotes_subgraphs))
}

#' @export
remotes_graph.integer <- function(x, ..., vs) {
  remotes_graph(vs[[x]])
}


#' @export
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
  remotes_tasks <- get_remotes_tasks(x)
  if (length(remotes_tasks) == 0) return(igraph::make_empty_graph())
  
  g <- star_graph(
    task = c(
      list(x),
      remotes_tasks
    )
  )
  
  # Recursively get remotes_tasks of remotes_tasks
  remotes_subgraphs <- lapply(remotes_tasks, remotes_graph)
  
  # TODO: To musze byc puste grafy a nie nulle
  graph_dedup_attrs(igraph::union(g, remotes_subgraphs))
}

#' @export
remotes_graph.check_task <- remotes_graph.install_task

get_remotes_tasks <- function(x) {
  UseMethod("get_remotes_tasks")
}

#' @export
get_remotes_tasks.default <- function(x) {
  NULL
}

#' @export
get_remotes_tasks.task <- function(x) {
  get_remotes_tasks(x$origin)
}

#' @export
get_remotes_tasks.pkg_origin_local <- function(x) {
  pkgs <- remotes:::extra_deps(devtools::as.package(x$source), "remotes")
  #pkgs <- pkgs[!vlapply(pkgs$remote, inherits, "cran_remote"), ]
  
  lapply(seq(length.out = NROW(pkgs)), function(i) {
    install_task(
      pkg_origin_remote(remote = pkgs$remote[[i]])
    )
  })
}

#' @export
get_remotes_tasks.pkg_origin_remote <- function(x) {
  x <- sanitize_pkg_origin_remote(x)
  get_remotes_tasks.pkg_origin_local(x)
}

get_remotes_package_source <- function(remote) {
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
