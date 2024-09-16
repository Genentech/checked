sequence_graph <- function(name, ...) {
  dots <- list(...)
  vertices <- data.frame(name = name)
  vertices[names(dots)] <- dots

  edges <- data.frame(
    from = utils::head(vertices$name, -1L),
    to = utils::tail(vertices$name, -1L)
  )

  igraph::graph_from_data_frame(
    edges,
    vertices = vertices
  )
}

merge_subgraphs <- function(gs) {
  es <- do.call(rbind, lapply(gs, igraph::as_data_frame))
  vs <- do.call(rbind, lapply(gs, igraph::as_data_frame, what = "vertices"))
  igraph::graph_from_data_frame(es, vertices = unique(vs))
}
