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

  igraph::graph_from_data_frame(es, vertices = vs)
}

complete_columns <- function(df, cols) {
  for (col in setdiff(cols, colnames(df))) df[[col]] <- NA
  df[, cols]
}
