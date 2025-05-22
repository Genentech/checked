vertex_df <- function(name, ...) {
  dots <- list(...)
  vertices <- data.frame(name = name)
  vertices[names(dots)] <- dots
  vertices[] <- lapply(vertices, `names<-`, NULL)
  vertices
}

sequence_graph <- function(..., name_by = ..1, name = hashes(name_by)) {
  vertices <- vertex_df(name = name, ...)
  edges <- data.frame(
    from = utils::head(vertices$name, -1L),
    to = utils::tail(vertices$name, -1L)
  )
  igraph::graph_from_data_frame(edges, vertices = vertices)
}

#' Deduplicate attributes
#'
#' Primarily intended for cleaning up the result of an [`igraph::union()`],
#' which adds duplicated attributes when attributes of the same name exist in
#' multiple graphs. Searches for suffixes and consolidates attributes,
#' taking the attribute from the first non-NA value observed.
#'
graph_dedup_attrs <- function(g) {
  # pattern appended to duplicated attributes
  re <- "_\\d+$"

  # de-duplicate vertex attributes
  v_attrs <- igraph::vertex_attr_names(g)
  v_dup_attrs <- grep(re, v_attrs, value = TRUE)
  v_dup_group <- sub(re, "", v_dup_attrs)
  v_dup_attrs <- split(v_dup_attrs, v_dup_group)
  for (i in seq_along(v_dup_attrs)) {
    attr_name <- names(v_dup_attrs[i])
    attr_value <- igraph::vertex_attr(g, v_dup_attrs[[i]][[1L]])
    g <- igraph::remove.vertex.attribute(g, v_dup_attrs[[i]][[1L]])
    for (attr_dup_name in v_dup_attrs[[i]][-1L]) {
      if (!anyNA(attr_value)) break
      is_na <- is.na(attr_value)
      attr_value[is_na] <- igraph::vertex_attr(g, attr_dup_name)[is_na]
      g <- igraph::remove.vertex.attribute(g, attr_dup_name)
    }
    g <- igraph::set_vertex_attr(g, attr_name, value = attr_value)
  }

  # de-duplicate edge attributes
  e_attrs <- igraph::edge_attr_names(g)
  e_dup_attrs <- grep(re, e_attrs, value = TRUE)
  e_dup_group <- sub(re, "", e_dup_attrs)
  e_dup_attrs <- split(e_dup_attrs, e_dup_group)
  for (i in seq_along(e_dup_attrs)) {
    attr_name <- names(e_dup_attrs[i])
    attr_value <- igraph::edge_attr(g, e_dup_attrs[[i]][[1L]])
    g <- igraph::remove.edge.attribute(g, e_dup_attrs[[i]][[1L]])
    for (attr_dup_name in e_dup_attrs[[i]][-1L]) {
      if (!anyNA(attr_value)) break
      is_na <- is.na(attr_value)
      attr_value[is_na] <- igraph::edge_attr(g, attr_dup_name)[is_na]
      g <- igraph::remove.edge.attribute(g, attr_dup_name)
    }
    g <- igraph::set_edge_attr(g, attr_name, value = attr_value)
  }

  g
}

complete_columns <- function(df, cols) {
  n_gtz <- nrow(df) > 0
  for (col in setdiff(cols, colnames(df))) df[[col]] <- NA[n_gtz]
  df[, cols, drop = FALSE]
}
