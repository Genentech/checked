#' Build task graph edges
#'
#' Edges describe relationships between tasks. Often, this is a dependency
#' between packages, requiring that some package be installed before a latter
#' task can be executed.
#'
#' [`tools::package_dependencies()`] is used to calculate these relationships.
#' However, the package data returned by [`utils::available.packages()`],
#' that is used internally to determine dependencies does not know about
#' local or remote packages, so those are first appended to this data set
#' prior to calculating edges. The bulk of this function serves to join this
#' data.
#'
#' @param x a `plan` object, containing a list of related steps.
#' @param repos `repos`, as expected by [`tools::package_dependencies()`] to
#'   determine package relationships.
#' @param ... params passed to helper methods.
#' @return A `data.frame` that can be used to build
#'   [`igraph::make_graph`] edges.
#'
#' @examples
#' \dontrun{
#  # requires that source code directory is for a package with revdeps
#' task_graph(plan_rev_dep_checks("."))
#' }
#' @keywords internal
#'
#' @importFrom igraph V E
task_graph <- function(x, repos = getOption("repos"), ...) {
  UseMethod("task_graph")
}

#' @export
task_graph.task <- function(x, repos = getOption("repos"), ...) {
  df <- pkg_deps(x$origin, repos = repos, dependencies = TRUE)
  # Distinguish direct dependencies of the package form possible indirect
  # in the same data.frame which could come from suggested loops. This ensures
  # there is a separate node for the root task.
  df$package[df$depth == "direct"] <-
    paste(df$package[df$depth == "direct"], "root", sep = "-")
  colmap <- c("package" = "from", "name" = "to")
  rename <- match(names(df), names(colmap))
  to_rename <- !is.na(rename)
  names(df)[to_rename] <- colmap[rename[to_rename]]
  df <- df[, c(which(to_rename), which(!to_rename)), drop = FALSE]
  g_dep <- igraph::graph_from_data_frame(df)

  E(g_dep)$relation <- RELATION$dep
  E(g_dep)$type <- DEP[E(g_dep)$type]
  V(g_dep)$task <- lapply(
    V(g_dep)$name,
    function(p) {
      if (endsWith(p, "-root")) {
        x
      } else {
        origin <- try_pkg_origin_repo(package = p, repos = repos)
        install_task(origin = origin)
      }
    }
  )

  V(g_dep)$name <- vcapply(V(g_dep)$task, as_vertex_name)

  g_dep
}

#' @export
task_graph.task_graph <- function(x, repos = getOption("repos"), ...) {
  # only use dependency edges when populating graph
  nodes <- V(x)[is_actionable_task(V(x)$task)]

  check_task_neighborhoods <- igraph::neighborhood(
    x,
    order = length(x),
    mode = "out",
    nodes = nodes
  )

  # for each check task in the plan, build a dependency tree and merge it
  # into the existing check task subtree
  check_task_neighborhoods <- lapply(check_task_neighborhoods, function(nh) {
    subtree <- igraph::induced_subgraph(x, nh)

    # build dependency graph, with fallback installation task
    deps <- task_graph(nh[[1]]$task, repos = repos)

    # merge trees on package names
    # NOTE: attributes (tasks) are preserved in the order they appear
    subtree <- graph_dedup_attrs(igraph::union(subtree, deps))

    # fill new edges with dependency relation - between check task and installs
    is_na <- is.na(E(subtree)$type)
    E(subtree)$type[is_na] <- DEP$Depends

    deduplicate_task_graph(subtree)
  })
  # then merge all the full check task task trees into a single graph
  g <- graph_dedup_attrs(igraph::union(x, check_task_neighborhoods))

  E(g)$type <- DEP[E(g)$type]
  V(g)$status <- STATUS$pending
  V(g)$process <- rep_len(list(), length(g))

  g <- task_graph_sort(g)

  # Restore the original (defined by the plan) order of primary tasks
  n_g <- length(V(g))
  x_ids <- as.numeric(V(g)[names(V(x))])
  g_ids <- numeric(n_g)
  g_ids[x_ids] <- seq(from = n_g, length.out = length(x_ids), by = -1)
  g_ids[g_ids == 0] <- setdiff(seq_along(V(g)), g_ids)
  g <- igraph::permute(g, g_ids)

  task_graph_class(g)
}


task_graph_class <- function(g) {
  class(g) <- c("task_graph", class(g))
  g
}


deduplicate_task_graph <- function(g) {
  vs <- V(g)
  for (i in vs) {
    # Task graph is allowed to have multi-edges in which case neighbors
    # would multiply the same nodes. Therefore we call unique.
    children <- unique(igraph::neighbors(g, i, "out"))
    children_names <- vcapply(children$task, package)
    duplicated_names <- children_names[duplicated(children_names)]
    for (p in duplicated_names) {
      duplicated_nodes <- children[children_names == p]
      g <- igraph::delete_edges(
        g,
        # Always keep lowest ID node
        paste(vs$name[[i]], duplicated_nodes[-1]$name, sep = "|")
      )
    }
  }
  isolated <- which(igraph::degree(g) == 0)
  igraph::delete_vertices(g, isolated)
}

dep_edges <- function(edges, dependencies = TRUE) {
  edges[edges$type %in% dependencies]
}

#' Find Task Neighborhood
#'
#' @param g A task graph, as produced with [task_graph()]
#' @param nodes Names or nodes objects of packages whose neighborhoods
#' should be calculated.
#'
#' @importFrom igraph neighborhood
#' @keywords internal
task_graph_neighborhoods <- function(g, nodes, ...) {
  igraph::neighborhood(
    g,
    order = length(g),
    nodes = nodes,
    mode = "out",
    ...
  )
}

#' Sort Task Graph by Strong Dependency Order
#'
#' @note
#' Cyclic dependencies are possible. Cyclic dependencies are disallowed for all
#' hard dependencies on CRAN today, though there have been historical instances
#' where they appeared on CRAN.
#'
#' Installation priority is based on:
#'   1. Total dependency footprint (low to high)
#'   2. Topology (leaf nodes first)
#'
#' @param g A [igraph::graph], expected to contain node attribute `type`.
#' @return The [igraph::graph] `g`, with vertices sorted in preferred
#'   installation order.
#'
#' @importFrom igraph vertex_attr neighborhood subgraph.edges permute topo_sort
#' @importFrom igraph E V E<- V<-
#' @keywords internal
task_graph_sort <- function(g) {
  roots <- which(is_check(V(g)$task))

  # calculcate check task neighborhood sizes
  nh_sizes <- igraph::neighborhood_size(
    g,
    nodes = roots,
    order = length(g),
    mode = "out"
  )

  # prioritize by neighborhood size (small to large)
  priority <- length(roots)
  priority_footprint <- integer(length(g))
  for (i in order(-nh_sizes)) {
    priority_footprint[roots[[i]]] <- priority
    priority <- priority - 1
  }

  # use only strong dependencies to prioritize by topology (leafs first)
  strong_edges <- dep_edges(E(g), check_dependencies("strong"))
  g_strong <- igraph::subgraph_from_edges(
    g,
    strong_edges,
    delete.vertices = FALSE
  )
  topo <- igraph::topo_sort(g_strong, mode = "out")
  priority_topo <- integer(length(g))
  priority_topo[match(topo$name, V(g)$name)] <- rev(seq_along(topo))

  # combine priorities, prioritize first by total, footprint then topology
  priority <- rbind(priority_footprint, priority_topo)
  order <- rank(length(V(g))^seq(nrow(priority) - 1, 0) %*% priority)
  g <- igraph::permute(g, order)

  g
}

#' Find the Next Packages Not Dependent on an Unavailable Package
#'
#' While other packages are in progress, ensure that the next selected package
#' already has its dependencies done.
#'
#' @details
#' There are helpers defined for particular use cases that strictly rely on the
#' [`task_graph_which_ready()`], they are:
#'
#' * `task_graph_update_check_ready()` - Updates check vertices whose all
#'   dependencies are satisfied.
#' * `task_graph_update_install_ready()` - Update install vertices whose all
#'   dependencies are satisfied.
#' * `task_graph_which_ready()` - List vertices whose wit ready status.
#'
#' @param g A dependency graph, as produced with [task_graph()].
#' @param v Names or nodes objects of packages whose satisfiability should be
#' checked.
#' @param dependencies Which dependencies types should be met for a node to be
#' considered satisfied.
#' @param status status name. Nodes in v fill be filtered to consists only nodes
#' with that status.
#'
#' @return The name of the next package to prioritize
#'
#' @importFrom igraph incident_edges tail_of
#' @keywords internal
task_graph_update_ready <- function(
  g,
  v = V(g),
  dependencies = TRUE,
  status = STATUS$pending
) {
  if (is.character(status)) {
    status <- STATUS[[status]]
  }

  dependencies <- check_dependencies(dependencies)

  if (length(status) > 0) {
    idx <- v$status %in% status
    v <- v[idx]
  }

  deps_met <- vlapply(
    igraph::incident_edges(g, v, mode = "out"),
    function(edges) {
      is_dep <- edges$type %in% dependencies
      all(igraph::head_of(g, edges[is_dep])$status == STATUS$done)
    }
  )

  task_graph_set_package_status(g, names(deps_met[deps_met]), STATUS$ready)
}

task_graph_update_check_ready <- function(
  g,
  ...,
  dependencies = "all",
  status = STATUS$pending
) {
  task_graph_update_ready(
    g,
    V(g)[is_check(V(g)$task)],
    ...,
    dependencies = dependencies,
    status = status
  )
}

task_graph_update_install_ready <- function(
  g,
  ...,
  dependencies = "strong",
  status = STATUS$pending
) {
  task_graph_update_ready(
    g,
    V(g)[is_install(V(g)$task)],
    ...,
    dependencies = dependencies,
    status = status
  )
}

#' Find nodes with ready state
#'
#' List nodes which have ready state prioritizing check task nodes over
#' install task nodes.
#'
#' @param g A dependency graph, as produced with [task_graph()].
#'
#' @return The names of packages with ready state.
#'
#' @keywords internal
task_graph_which_ready <- function(g) {
  nodes <- V(g)[is_actionable_task(V(g)$task)]
  statuses <- igraph::vertex.attributes(g, nodes)$status
  nodes[statuses == STATUS["ready"]]
}

task_graph_set_package_status <- function(g, v, status) {
  if (is.character(status)) status <- STATUS[[status]]
  igraph::set_vertex_attr(g, "status", v, status)
}

task_graph_package_status <- function(g, v) {
  igraph::vertex.attributes(g, v)$status
}

`task_graph_package_status<-` <- function(x, v, value) {
  task_graph_set_package_status(x, v, value)
}

`task_graph_task_process<-` <- function(x, v, value) {
  task_graph_set_task_process(x, v, value)
}


task_graph_task <- function(g, v) {
  igraph::vertex_attr(g, "spec", v)[[1]]
}

task_graph_task_name <- function(g, v) {
  igraph::vertex_attr(g, "name", v)[[1]]
}

task_graph_task_process <- function(g, v) {
  igraph::vertex_attr(g, "process", v)[[1]]
}

task_graph_set_task_process <- function(g, v, process) {
  igraph::set_vertex_attr(g, "process", v, list(process))
}

task_graph_update_done <- function(g, lib.loc) {
  v <- V(g)[V(g)$type == "install"]
  which_done <- which(vlapply(v$name, is_package_installed, lib.loc = lib.loc))
  task_graph_set_package_status(g, v[which_done], STATUS$done)
}

#' @export
plot.task_graph <- function(x, ..., interactive = FALSE) {
  style <- function(attr, ...) {
    map <- simplify2array(list(...))
    nomatch <- if (names(map[length(map)]) == "") length(map) else NA
    map[match(as.character(attr), names(map), nomatch = nomatch)]
  }

  task_type <- vcapply(V(x)$task, function(task) {
    if (is_install(task)) return(class(task$origin)[[1]])
    class(task)[[1]]
  })

  vertex <- igraph::as_data_frame(x, what = "vertices")
  edge <- igraph::as_data_frame(x, what = "edges")

  vertex$color <- style(
    task_type,
    "rev_dep_dep_meta_task" = "blueviolet",
    "rev_dep_check_meta_task" = "blueviolet",
    "check_task" = "lightblue",
    "library_task" = "lightgreen",
    "install_task" = "cornflowerblue",
    "pkg_origin_repo" = "cornflowerblue",
    "pkg_origin_base" = "lightgray",
    "pkg_origin_unknown" = "red",
    "pkg_origin_local" = "blue",
    "pkg_origin_remote" = "orange",
    "red"
  )

  vertex$label <- cli::ansi_strip(vcapply(V(x)$task, format, g = x))

  vertex$frame.color <- style(
    STATUS[V(x)$status %||% 1],
    "done" = "green",
    "in progress" = "lightgreen",
    "black"
  )

  vertex$frame.width <- style(
    STATUS[V(x)$status %||% 1],
    "done" = 3L,
    1L
  )

  vertex$size <- style(
    task_type,
    "install_task" = 5,
    "pkg_origin_repo" = 5,
    "pkg_origin_unknown" = 5,
    "pkg_origin_local" = 5,
    "pkg_origin_base" = 5,
    8
  )

  if (interactive) {
    plot_interactive_task_graph(x, vertex = vertex, edge = edge)
  } else {
    plot_static_task_graph(x, vertex = vertex, edge = edge)
  }
}

plot_static_task_graph <- function(
  g,
  vertex = igraph::as_data_frame(g, what = "vertices"),
  edge = igraph::as_data_frame(g, what = "edges")
) {
  igraph::plot.igraph(
    g,
    vertex.label.family = "sans",
    vertex.label.color = "gray4",
    vertex.label.dist = 1,
    vertex.label = vertex$label,
    vertex.color = vertex$color,
    vertex.size = vertex$size,
    vertex.frame.color = vertex$frame.color,
    vertex.frame.width = vertex$frame.width,
    layout = igraph::layout_with_sugiyama(
      g,
      hgap = 200,
      maxiter = 1000
    )
  )
}

plot_interactive_task_graph <- function(
  g,
  vertex = igraph::as_data_frame(g, what = "vertices"),
  edge = igraph::as_data_frame(g, what = "edges")
) {
  vertex$title <- gsub("<|>", "", vcapply(vertex$task, format))
  edge_titles <- paste0(edge$type, ifelse(
    vlapply(edge$version, is.na),
    "",
    paste0(
      "(",
      ifelse(is.na(edge$op), "", paste0(edge$op, " ")),
      format(edge$version), ")"
    )
  ))

  if (length(edge_titles) > 0) edge$title <- edge_titles
  vertex$id <- seq_len(nrow(vertex))
  edge$from <- vertex$id[match(edge$from, vertex$name)]
  edge$to <- vertex$id[match(edge$to, vertex$name)]
  edge$arrows <- "to"

  # visNetwork hates non-atomic vectors
  vertex$task <- NULL
  edge$version <- format(edge$version)

  visNetwork::visNetwork(
    nodes = vertex,
    edges = edge,
    width = "100vw",
    height = "100vh",
    tooltipDelay = 0
  )
}
