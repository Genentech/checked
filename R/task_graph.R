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
#' @param plan a `plan` object, containing a list of related steps.
#' @param repos `repos`, as expected by [`tools::package_dependencies()`] to
#'   determine package relationships.
#' @return A `data.frame` that can be used to build [`igraph`] edges.
#'
#' @examples
#' \dontrun{
#  # requires that source code directory is for a package with revdeps
#' task_graph_create(plan_rev_dep_checks("."))
#' }
#' @keywords internal
task_graph_create <- function(plan, repos = getOption("repos")) {
  plan_tasks <- igraph::V(plan)[igraph::V(plan)$task_type == "check_task"]
  plan_neighborhoods <- igraph::neighborhood(
    plan,
    order = length(plan),
    mode = "out",
    nodes = plan_tasks
  )

  # for each check task in the plan, build a dependency tree and merge it
  # into the existing check task subtree
  plan_neighborhoods <- lapply(plan_neighborhoods, function(nh) {
    subtree <- igraph::induced_subgraph(plan, nh)
    deps <- dep_tree(nh[[1]]$task)
    igraph::reverse_edges(deps)

    subtree <- graph_project(
      x = deps,
      onto = subtree,
      where = c("name" = "package")
    )

    # set missing dependencies to be installed from repo
    missing_task <- is.na(igraph::V(subtree)$task)
    igraph::V(subtree)$task[missing_task] <- lapply(
      igraph::V(subtree)$package[missing_task],
      function(package) {
        origin <- try_pkg_origin_repo(package = package, repos = repos)
        install_task(origin = origin)
      }
    )

    # re-hash tasks as vertex names (populate missing vertex names)
    igraph::V(subtree)$name <- vcapply(igraph::V(subtree)$task, hash)
    igraph::V(subtree)$task_type <- vcapply(
      igraph::V(subtree)$task,
      function(task) class(task)[[1]]
    )

    subtree
  })

  # then merge all the full check task task trees into a single graph
  g <- merge_subgraphs(plan_neighborhoods)
  class(g) <- c("task_graph", class(g))

  igraph::V(g)$status <- STATUS$pending
  igraph::V(g)$process <- rep_len(list(), length(g))
  igraph::E(g)$plan[is.na(igraph::E(g)$plan)] <- PLAN$inferred

  task_graph_sort(g)
}

lib_node_tasks <- function(g, nodes) {
  install_nodes <- igraph::adjacent_vertices(g, nodes, mode = "out")
  lapply(install_nodes, function(nodes) {
    nodes$task[is_install(nodes$task)]
  })
}

lib_node_pkgs <- function(g, nodes) {
  install_nodes <- igraph::adjacent_vertices(g, nodes, mode = "out")
  lapply(install_nodes, function(nodes) {
    tasks <- nodes$task[is_install(nodes$task)]
    vcapply(tasks, function(task) task$origin$package)
  })
}

#' Project one graph onto another
#'
#' Project graph `x` onto graph `onto`, making a single graph with merged
#' vertices where the attributes in `where` from `onto` map to the associated
#' attributes in `x` given by the names of `where`. `where` _must_ contain
#' an element called `name`, used to map vertices.
#'
#' The resulting graph will have the combined vertices and edges of both graphs.
#'
graph_project <- function(x, onto, where = c("name" = "name")) {
  igraph::vertex.attributes(x)[where] <-
    igraph::vertex.attributes(x)[names(where)]

  projection <- match(
    igraph::V(x)$name,
    igraph::vertex_attr(onto, where[["name"]])
  )

  igraph::V(x)$name <- ifelse(
    is.na(projection),
    seq_along(igraph::V(x)),
    igraph::V(onto)$name[projection]
  )

  merge_subgraphs(list(onto, x))
}

package_graph <- function(db, packages = db[, "Package"], dependencies = TRUE) {
  dependencies <- as_pkg_dependencies(dependencies)

  direct_deps <- unique(as.character(unlist(package_deps(
    packages,
    db = db,
    which = dependencies$direct,
    recursive = FALSE
  ))))

  indirect_deps <- unique(as.character(unlist(package_deps(
    c(packages, direct_deps),
    db = db,
    which = dependencies$indirect,
    recursive = TRUE
  ))))

  deps <- unique(c(packages, direct_deps, indirect_deps))
  deps <- deps[!deps %in% base_pkgs() & deps %in% db[, "Package"]]

  edges <- packages_edges(db[deps, ])
  vertices <- data.frame(name = unique(c(edges$package, edges$dep)))
  igraph::graph_from_data_frame(edges, vertices = vertices)
}

#' Produce Graph Edges from Packages Index
#'
#' @param ap `matrix`, as produced by [`utils::available.packages()`]
#' @return `data.frame` with columns `package`, `dep` and `type` and
#'   one row for each dependency relationship.
#'
packages_edges <- function(ap) {
  deps_by_type <- lapply(names(DEP), function(deptype) {
    is_na <- is.na(ap[, deptype])

    # filter for available packages with at least one dep of deptype
    deps <- ap[!is_na, deptype]
    names(deps) <- ap[!is_na, "Package"]

    # split deps of deptype
    deps <- lapply(deps, .tools$.split_dependencies)

    # and structure
    data.frame(
      package = rep(names(deps), times = viapply(deps, length)),
      dep = unlist(lapply(deps, names), use.names = FALSE),
      type = deptype
    )
  })

  do.call(rbind, deps_by_type)
}

task_graph_vertices <- function(plan, df, edges, repos) {
  vertices <- unique(c(edges$dep, edges$root))
  custom_pkgs_aliases <- uulist(lapply(df$custom, `[[`, "alias"))
  task_type <- ifelse(vertices %in% df$alias, "check", "install")

  spec <- lapply(vertices, function(v) {
    if (v %in% df$alias) {
      df$package[[which(df$alias == v)]]
    } else if (v %in% custom_pkgs_aliases) {
      df$custom[[utils::head(which(as.character(lapply(df$custom, `[[`, "alias")) == v), 1)]]
    } else {
      install_task(
        alias = v,
        package = pkg_origin_repo(name = v, repos = repos)
      )
    }
  })

  out <- data.frame(
    name = vertices,
    type = task_type,
    custom = vertices %in% custom_pkgs_aliases
  )

  out$spec <- spec
  out
}

#' Find Task Neighborhood
#'
#' @param g A task graph, as produced with [task_graph_create()]
#' @param nodes Names or nodes objects of packages whose neighborhoods
#' should be calculated.
#'
#' @importFrom igraph neighborhood
#' @keywords internal
task_graph_neighborhoods <- function(g, nodes) {
  igraph::neighborhood(
    g,
    order = length(g),
    nodes = nodes,
    mode = "out"
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
  roots <- which(igraph::vertex_attr(g, "type") == "check")

  # split into neighborhoods by root (revdep)
  nhood <- task_graph_neighborhoods(g, roots)

  # prioritize by neighborhood size (small to large)
  priority <- length(nhood)
  priority_footprint <- integer(length(g))
  for (i in order(-vapply(nhood, length, integer(1L)))) {
    priority_footprint[nhood[[i]]] <- priority
    priority <- priority - 1
  }

  # use only strong dependencies to prioritize by topology (leafs first)
  strong_edges <- igraph::E(g)[igraph::E(g)$type %in% DEP_STRONG]
  g_strong <- igraph::subgraph.edges(g, strong_edges, delete.vertices = FALSE)
  topo <- igraph::topo_sort(g_strong, mode = "in")
  priority_topo <- integer(length(g))
  priority_topo[match(topo$name, igraph::V(g)$name)] <- rev(seq_along(topo))

  # combine priorities, prioritize first by total, footprint then topology
  priority <- rbind(priority_footprint, priority_topo)
  order <- rank(length(igraph::V(g))^seq(nrow(priority) - 1, 0) %*% priority)
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
#' [`task_graph_which_satisfied()`], they are:
#'
#' * `task_graph_which_satisfied_strong()` - List vertices whose strong
#'   dependencies are satisfied.
#' * `task_graph_which_check_satisfied()` - List root vertices whose all
#'   dependencies are satisfied.
#' * `task_graph_which_install_satisfied()` - List install vertices whose
#'   dependencies are all satisfied
#'
#' @param g A dependency graph, as produced with [task_graph_create()].
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
task_graph_which_satisfied <- function(
  g,
  v = igraph::V(g),
  dependencies = TRUE,
  status = STATUS$pending
) {
  if (is.character(status)) status <- STATUS[[status]]
  dependencies <- check_dependencies(dependencies)
  if (length(status) > 0) {
    idx <- v$status %in% status
    v <- v[idx]
  }
  deps_met <- vlapply(
    igraph::incident_edges(g, v, mode = "in"),
    function(edges) {
      edges <- edges[edges$type %in% dependencies]
      all(igraph::tail_of(g, edges)$status == STATUS$done)
    }
  )
  names(deps_met[deps_met])
}

task_graph_which_satisfied_strong <- function(..., dependencies = "strong") { # nolint
  task_graph_which_satisfied(..., dependencies = dependencies)
}

task_graph_which_check_satisfied <- function(
  g,
  ...,
  dependencies = "all",
  status = STATUS$pending
) {
  task_graph_which_satisfied(
    g,
    igraph::V(g)[vlapply(igraph::V(g)$task, is_check)],
    ...,
    dependencies = dependencies,
    status = status
  )
}

task_graph_which_install_satisfied <- function(
  g,
  ...,
  dependencies = "strong",
  status = STATUS$pending
) {
  task_graph_which_satisfied(
    g,
    igraph::V(g)[is_install(igraph::V(g)$task)],
    ...,
    dependencies = dependencies,
    status = status
  )
}

empty_edge <- data.frame(
  dep = character(0),
  root = character(0),
  type = character(0)
)

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
  v <- igraph::V(g)[igraph::V(g)$type == "install"]
  which_done <- which(vlapply(v$name, is_package_installed, lib.loc = lib.loc))
  task_graph_set_package_status(g, v[which_done], STATUS$done)
}

is_package_done <- function(pkg, lib.loc) {
  path <- find.package(pkg, lib.loc = lib.loc, quiet = TRUE)
  length(path) > 0
}

#' @export
as_visNetwork <- function(x, ...) {
  color_by_task_type <- c(
    "check_task" = "lightblue",
    "library_task" = "lightgreen",
    "install_task" = "cornflowerblue",
    "pkg_origin_repo" = "cornflowerblue",
    "pkg_origin_unknown" = "red",
    "pkg_origin_local" = "blue",
    "red"
  )

  nodes <- igraph::as_data_frame(x, what = "vertices")
  task_type <- vcapply(igraph::V(x)$task, function(task) {
    if (inherits(task, "install_task")) return(class(task$origin)[[1]])
    class(task)[[1]]
  })

  nodes$id <- nodes$name

  nodes$label <- vcapply(igraph::V(g)$task, function(task) {
    friendly_name(task, short = TRUE)
  })

  nodes$color <- color_by_task_type[match(
    task_type,
    names(color_by_task_type),
    nomatch = length(color_by_task_type)
  )]

  edges <- igraph::as_data_frame(x)

  nodes$task <- NULL  # work around for visNetwork infinite recursion issue?
  visNetwork::visNetwork(nodes = nodes, edges = edges)
}

#' @export
plot.task_graph <- function(x, ...) {
  style <- function(attr, ...) {
    map <- simplify2array(list(...))
    nomatch <- if (names(map[length(map)]) == "") length(map) else NA
    map[match(attr, names(map), nomatch = nomatch)]
  }

  task_type <- vcapply(igraph::V(x)$task, function(task) {
    if (is_install(task)) return(class(task$origin)[[1]])
    class(task)[[1]]
  })

  vertex.color <- style(
    task_type,
    "check_task" = "lightblue",
    "library_task" = "lightgreen",
    "install_task" = "cornflowerblue",
    "pkg_origin_repo" = "cornflowerblue",
    "pkg_origin_base" = "lightgray",
    "pkg_origin_unknown" = "red",
    "pkg_origin_local" = "blue",
    "red"
  )

  vertex.label <- vcapply(
    igraph::V(x)$task,
    friendly_name,
    short = TRUE
  )

  vertex.size <- style(
    task_type,
    "install_task" = 5,
    "pkg_origin_repo" = 5,
    "pkg_origin_unknown" = 5,
    "pkg_origin_local" = 5,
    "pkg_origin_base" = 5,
    8
  )

  edge.lty <- style(
    igraph::E(x)$plan,
    "planned" = 1,  # solid
    "inferred" = 3  # dotted
  )

  tail_ids <- igraph::ends(x, igraph::E(x))[, 2L]
  is_inst <- is_install(igraph::V(x)[tail_ids]$task)
  igraph::E(x)$weights <- 1 + !is_inst * 9

  igraph::plot.igraph(
    x,
    vertex.label.family = "sans",
    vertex.label.color = "gray4",
    vertex.label.dist = 1,
    vertex.label = vertex.label,
    vertex.color = vertex.color,
    vertex.size = vertex.size,
    edge.lty = edge.lty
  )
}
