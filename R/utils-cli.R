#' task formatter bindings
#'
#' This bit of code is intended for use with [`cli_task()`], and allows for us
#' to layer symbol bindings on top of the environment used for string
#' interpolation which provide syntactic sugar for common formatting components.
#'
task_formats <- function(
  g = NULL,
  nodes = V(g),
  task = NULL,
  tasks = list(task)
) {
  if (is.null(task)) {
    task <- nodes$task
  }

  # # NOTE: currently unused, vestigial vectorized alternative
  # if (length(tasks) == 1 && is.null(tasks[[1]])) {
  #   tasks <- V(g)$task
  # }

  makeActiveBinding("source", env = environment(), function() {
    src <- task$origin %||% character(0L)
    src <- format(src)
    if (!is.null(names(src))) src <- names(src)
    cli_type("path", src)
  })

  makeActiveBinding("source.full", env = environment(), function() {
    src <-  task$origin$source %||% character(0L)
    cli_type("path", format(src))
  })

  makeActiveBinding("source.type", env = environment(), function() {
    src_type <- class(task$origin)[[1]]
    src_type_str <- switch(src_type,
      "pkg_origin_local" = "local",
      "pkg_origin_repo" = source,
      "remote"
    )

    cli_type("path", format(src_type_str))
  })

  makeActiveBinding("package", env = environment(), function() {
    pkg <- task$origin$package %||% character(0L)
    cli_type("package", pkg)
  })

  makeActiveBinding("version", env = environment(), function() {
    cli_type("version", format(task$origin$version %||% character(0L)))
  })

  makeActiveBinding("action", env = environment(), function() {
    cli_type("task_type", format_task_type(task))
  })

  makeActiveBinding("dep.sources", env = environment(), function() {
    # get nodes dependencies
    dep_edges <- E(g)$relation == as.numeric(RELATION$dep)
    dep_g <- igraph::subgraph_from_edges(g, which(dep_edges))
    vs <- task_graph_neighborhoods(dep_g, nodes = nodes$name, mindist = 1)
    vs <- unlist(vs)

    # get dependency source names
    sources <- lapply(V(g)[vs]$task, function(task) format(task$origin))
    sources <- unlist(Filter(length, sources))

    if (!is.null(names(sources))) {
      is_unnamed <- names(sources) == ""
      names(sources[is_unnamed]) <- sources[is_unnamed]
    }

    # filter for only novel dependencies
    sources <- setdiff(names(sources), names(getOption("repos")))
    cli_type("dep_sources", sources)
  })

  environment()
}

#' Produce cli output for a task
#'
#' Provided a task, allows for use of a handful of shorthand symbols which will
#' use the task as a context for formatting task fields.
#'
#' @examples
#' task <- install_task(origin = pkg_origin(
#'   package = "pkg",
#'   version = package_version("1.2.3"),
#'   source = "../../Programming/options"
#' ))
#'
#' fmt(task = task, "{action} {package} ({version}) from {source}")
#'
fmt <- function(..., g, nodes, task = NULL, .envir = parent.frame()) {
  env <- task_formats(g = g, nodes = nodes, task = task, tasks = NULL)
  parent.env(env) <- .envir

  cli::cli_div(
    theme = list(
      div = list(
        "class-map" = list(
          "cli_path" = "path",
          "cli_task_type" = "class",
          "cli_package" = "pkg",
          "cli_version" = "pkg-version"
        )
      ),
      ".pkg-version" = list(
        "font-style" = "italic",
        "transform" = function(x) sprintf("v%s", x)
      )
    )
  )

  cli::format_inline(..., .envir = env)
}

glu <- function(..., g, nodes, task = NULL, .envir = parent.frame()) {
  env <- task_formats(g = g, nodes = nodes, task = task, tasks = NULL)
  parent.env(env) <- .envir
  glue::glue(..., .envir = env)
}

#' Create a 'cli' Spinner With Suppressed Output
#'
#' 'cli' will implicitly push spinner output to various output streams,
#' affecting the terminal cursor position. To allow for a terminal interface
#' that has spinners above the last line, this function suppresses the output
#' and simply returns its frame contents.
#'
#' @param ... passed to [cli::make_spinner]
#' @param stream passed to [cli::make_spinner], defaults to a null file device
#' @return A interface similar to a 'cli' spinner, but with suppressed output
#'
#' @importFrom cli make_spinner
#' @keywords internal
silent_spinner <- function(..., stream = devnull()) {
  spinner <- cli::make_spinner(..., stream = stream)

  spin <- function(...) {
    spinner$spin(...)
    with(environment(spinner$spin), c_spinner$frames[[c_state]])
  }

  list(spin = spin, final = spinner$final)
}

#' Reuse or Create A Null File Connection
#' @keywords internal
devnull <- function() {
  cons <- showConnections(all = TRUE)[, "description"]
  if (length(w <- which(nullfile() == cons))) {
    getConnection(names(cons)[[w[[1]]]])
  } else {
    file(nullfile())
  }
}

format_time <- function(x) {
  n <- unclass(x)
  sprintf("%.01f%s", n, substring(attr(n, "units"), 1, 1))
}

str_pad <- function(x, n) {
  x <- format(x)
  paste0(strrep(" ", n - nchar(x)), x)
}

emoji <- list(
  dev = "\U0001F6A7",
  release = "\U0001F680"
)
