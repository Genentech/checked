vertex_df <- function(name, ...) {
  dots <- list(...)
  vertices <- data.frame(name = name)
  vertices[names(dots)] <- dots
  vertices[] <- lapply(vertices, `names<-`, NULL)
  vertices
}

sequence_graph <- function(name, ...) {
  vertices <- vertex_df(name = name, ...)
  edges <- data.frame(
    from = utils::head(vertices$name, -1L),
    to = utils::tail(vertices$name, -1L)
  )
  igraph::graph_from_data_frame(edges, vertices = vertices)
}

merge_subgraphs <- function(gs) {
  edfs <- lapply(gs, igraph::as_data_frame)
  all_cols <- unique(unlist(lapply(edfs, colnames)))
  edfs <- lapply(edfs, complete_columns, all_cols)
  es <- unique(do.call(rbind, edfs))
  rownames(es) <- NULL

  vdfs <- lapply(gs, igraph::as_data_frame, what = "vertices")
  all_cols <- unique(unlist(lapply(vdfs, colnames)))
  vdfs <- lapply(vdfs, complete_columns, all_cols)
  vs <- do.call(rbind, vdfs)
  vs <- vs[!duplicated(vs$name), ]
  rownames(vs) <- NULL

  g <- igraph::graph_from_data_frame(es, vertices = vs)

  # re-assign vertex and edge attributes to preserve class
  igraph::vertex.attributes(g) <- as.list(vs)
  igraph::edge.attributes(g) <- as.list(es[, -(1:2), drop = FALSE])

  g
}

complete_columns <- function(df, cols) {
  n_gtz <- nrow(df) > 0
  for (col in setdiff(cols, colnames(df))) df[[col]] <- NA[n_gtz]
  df[, cols, drop = FALSE]
}
