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
  # NOTE: igraph::as_data_frame will coerce factor variables to character,
  # reassign columns from attributes to avoid coercion

  edfs <- lapply(gs, function(g) {
    df <- igraph::as_data_frame(g)
    attrs <- igraph::edge.attributes(g)
    df[names(attrs)] <- attrs
    df
  })

  all_cols <- unique(unlist(lapply(edfs, colnames)))
  edfs <- lapply(edfs, complete_columns, all_cols)
  es <- unique(do.call(rbind, edfs))
  rownames(es) <- NULL

  vdfs <- lapply(gs, function(g) {
    df <- igraph::as_data_frame(g, what = "vertices")
    attrs <- igraph::vertex.attributes(g)
    df[names(attrs)] <- attrs
    df
  })

  all_cols <- unique(unlist(lapply(vdfs, colnames)))
  vdfs <- lapply(vdfs, complete_columns, all_cols)
  vs <- do.call(rbind, vdfs)
  vs <- vs[!duplicated(vs$name), ]
  rownames(vs) <- NULL

  g <- igraph::graph_from_data_frame(es, vertices = vs)

  # re-assign vertex and edge attributes to preserve class
  igraph::vertex.attributes(g) <- vs
  igraph::edge.attributes(g) <- es[, -(1:2), drop = FALSE]

  g
}

complete_columns <- function(df, cols) {
  n_gtz <- nrow(df) > 0
  for (col in setdiff(cols, colnames(df))) df[[col]] <- NA[n_gtz]
  df[, cols, drop = FALSE]
}
