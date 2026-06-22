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
#' taking the attribute from the first non-NA value observed. The function
#' rebuilds the graph from scratch as accessing attributes once, operating on
#' lists and then assigning them to a new graph is significantly faster than
#' manipulating attribiutes of the existing graph.
#'
#' @param g task_graph object
#'
#' @keywords internal
graph_dedup_attrs <- function(g) {
  re <- "_\\d+$"
  v_all <- igraph::vertex_attr(g)
  e_all <- igraph::edge_attr(g)
  v_attr_names <- names(v_all)
  e_attr_names <- names(e_all)
  v_dup <- grep(re, v_attr_names, value = TRUE)
  e_dup <- grep(re, e_attr_names, value = TRUE)

  # Nothing to deduplicate
  if (length(v_dup) == 0 && length(e_dup) == 0) return(g)

  first_non_na <- function(list) {
    out <- list[[1]]
    if (!any(is.na(out))) return(out)

    for (i in seq(2, length(list))) {
      is_na <- is.na(out)
      if (any(is_na)) out[is_na] <- list[[i]][is_na]
    }
    out
  }

  groups <- split(v_dup, sub(re, "", v_dup))
  for (base in names(groups)) {
    cols <- groups[[base]]
    v_all[[base]] <- first_non_na(v_all[cols])
  }
  v_all[v_dup] <- NULL

  groups <- split(e_dup, sub(re, "", e_dup))
  for (base in names(groups)) {
    cols <- groups[[base]]
    e_all[[base]] <- first_non_na(e_all[cols])
  }
  e_all[e_dup] <- NULL

  vertices <- data.frame(name = V(g)$name)

  for (attr in setdiff(names(v_all), "name")) {
    vertices[[attr]] <- v_all[[attr]]
  }

  # Build edges data.frame (keeps current edge order)
  el <- igraph::as_edgelist(g, names = TRUE)
  edges <- data.frame(
    from = el[, 1],
    to = el[, 2]
  )

  for (attr in names(e_all)) {
    edges[[attr]] <- e_all[[attr]]
  }

  # Rebuild graph
  g_rebuilt <- igraph::graph_from_data_frame(
    d = edges,
    directed = TRUE,
    vertices = vertices
  )

  class(g_rebuilt) <- class(g)
  g_rebuilt
}
