vertex_df <- function(name, ...) {
  dots <- list(...)
  vertices <- data.frame(name = name)
  vertices[names(dots)] <- dots
  vertices[] <- lapply(vertices, `names<-`, NULL)
  vertices
}

new_graph <- function(
  ...,
  vertex_attrs = list(...),
  edge_attrs = list(),
  name_by = vertex_attrs[[1]],
  name = vcapply(name_by, as_vertex_name),
  type = c("sequence", "star")
) {
  type <- match.arg(type)
  vertices <- vertex_df(name = name, ...)
  edges_orientation <- switch(
    type,
    sequence = -1L,
    star = 1
  )
  edges <- data.frame(
    from = utils::head(vertices$name, edges_orientation),
    to = utils::tail(vertices$name, -1L)
  )
  if (length(edge_attrs) > 0) {
    edges <- cbind(edges, edge_attrs)
  }
  igraph::graph_from_data_frame(edges, vertices = vertices)
}

sequence_graph <- function(...) {
  new_graph(..., type = "sequence")
}

star_graph <- function(...) {
  new_graph(..., type = "star")
}

#' Deduplicate attributes
#'
#' Primarily intended for cleaning up the result of an [`igraph::union()`],
#' which adds duplicated attributes when attributes of the same name exist in
#' multiple graphs. Searches for suffixes and consolidates attributes,
#' taking the attribute from the first non-NA value observed.
#'
#' @param g task_graph object
#'
#' @keywords internal
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
    g <- igraph::delete_vertex_attr(g, v_dup_attrs[[i]][[1L]])
    for (attr_dup_name in v_dup_attrs[[i]][-1L]) {
      is_na <- is.na(attr_value)
      attr_value[is_na] <- igraph::vertex_attr(g, attr_dup_name)[is_na]
      g <- igraph::delete_vertex_attr(g, attr_dup_name)
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
    g <- igraph::delete_edge_attr(g, e_dup_attrs[[i]][[1L]])
    for (attr_dup_name in e_dup_attrs[[i]][-1L]) {
      is_na <- is.na(attr_value)
      attr_value[is_na] <- igraph::edge_attr(g, attr_dup_name)[is_na]
      g <- igraph::delete_edge_attr(g, attr_dup_name)
    }
    g <- igraph::set_edge_attr(g, attr_name, value = attr_value)
  }

  g
}
