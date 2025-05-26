report_task <- function(reporter, g, v) {
  UseMethod("report_task")
}

#' @export
report_task.reporter_ansi_tty <- function(reporter, g, v) {
  UseMethod("report_task_ansi_tty", v$task)
}

report_task_ansi_tty <- report_task.reporter_ansi_tty

format_status_line_ansi <- function(process, ...) {
  UseMethod("format_status_line_ansi")
}

#' @export
format_status_line_ansi.check_process <- function(
    process,
    ...,
    width = getOption("width", 80L)) {
  checks <- process$get_checks()

  # runtime of process
  process_time <- paste0(format_time(process$get_duration()), " ")

  # runtime of current check (only displayed if >30s)
  check_time <- Sys.time() - process$get_time_last_check_start()
  if (length(check_time) == 0 || check_time < difftime(30, 0)) {
    check_time <- ""
  } else {
    check_time <- cli::col_grey("(", format_time(check_time), ") ")
  }

  msg <- ""
  status <- max(as.numeric(checks), -1)
  if (length(checks) == 0) {
    # have not hit checks yet
    msg <- "starting ..."
    status <- process$spin()
  } else if (process$is_alive()) {
    # processing checks
    msg <- paste("checking", names(utils::tail(checks, 1)), "...")
    status <- process$spin()
    process_time <- cli::col_cyan(process_time)
  } else {
    # done
    process_time <- cli::col_grey(process_time)
  }

  msg <- cli::format_inline("{process_time}{check_time}{msg}")
  counts <- table(process$get_checks())
  out <- cli_table_row(
    status = status,
    ok = counts[["NONE"]] + counts[["OK"]],
    notes = counts[["NOTE"]],
    warnings = counts[["WARNING"]],
    errors = counts[["ERROR"]],
    msg
  )

  cli::ansi_substring(out, 1, width)
}

#' @export
format_status_line_ansi.default <- function(
    process,
    ...,
    width = getOption("width", 80L)) {
  out <- cli_table_row(
    status = "NONE",
    ok = 0,
    notes = 0,
    warnings = 0,
    errors = 0,
    "restored from system file."
  )

  cli::ansi_substring(out, 1, width)
}

#' @export
reporter_line <- function(label, status, style = NA_character_) {
  structure(
    list(label = label, status = status, style = style),
    class = "reporter_line"
  )
}

#' @export
format.reporter_line <- function(x, width = cli::console_width()) {
  rule <- switch(x$style,
    "h1" = "\u2550",
    " "
  )

  label <- format(x$label, pad = rule)

  status <- switch(
    x$style,
    "h1" = gsub(" ", rule, cli_table_row(
      ok = "",
      notes = "",
      warnings = "",
      errors = "",
      style = "header",
      symbols = list(bar = "\u256A")
    )),
    x$status
  )

  out <- paste0(label, status, strrep(rule, width))
  out <- cli::ansi_substr(out, 1, width)

  out
}

#' @export
reporter_cell <- function(
  content,
  justify = "left",
  padding = c(0, 0),
  width = Inf
) {
  structure(
    content,
    justify = justify,
    padding = padding,
    width = width,
    class = "reporter_cell"
  )
}

#' @export
format.reporter_cell <- function(x,
  padding = attr(x, "padding") %||% c(0, 0),
  justify = attr(x, "justify") %||% "right",
  width = attr(x, "width") %||% cli::console_width(),
  pad = " "
) {
  n <- width - sum(padding) - cli::ansi_nchar(x)
  paste0(
    if (!is.null(justify) && justify == "right") strrep(pad, n),
    if (!is.null(justify) && justify == "center") strrep(pad, (n - n %/% 2)),
    strrep(pad, padding[[1]]),
    x,
    strrep(pad, padding[[2]]),
    if (!is.null(justify) && justify == "center") strrep(pad, n %/% 2),
    if (!is.null(justify) && justify == "left") strrep(pad, n)
  )
}

#' @export
report_task_ansi_tty.rev_dep_check_meta_task <- function(reporter, g, v) {
  # package being rev-dep-check'ed
  package <- v$task$origin$package

  # subset for dependency edges from rev dep task
  dep_edges <- E(g)$relation == as.numeric(RELATION$dep)
  dep_g <- igraph::subgraph_from_edges(g, which(dep_edges))

  # get individual rev dep checks, and get their trees
  check_nodes <- igraph::neighbors(dep_g, v$name, "out")
  check_neighborhoods <- igraph::make_neighborhood_graph(
    dep_g,
    order = length(dep_g),
    check_nodes,
    mode = "out"
  )

  # TODO: would be nice to reintroduce `package` as a vertex attribute, maybe
  #   remove from `pkg_origin`?
  package_tasks <- lapply(
    check_neighborhoods,
    function(g) {
      is_dep <- vlapply(V(g)$task, function(i) i$origin$package == package)
      V(g)[is_dep][[1]]$task
    }
  )

  package_task_labels <- lapply(
    package_tasks,
    function(t) {
      reporter_cell(
        fmt(task = t, "+{package} {version} {source.type}"),
        width = reporter$label_nchar
      )
    }
  )

  rev_dep_check_nodes <- get_reporter_node(g, v, mode = "in")
  rev_dep <- fmt(task = rev_dep_check_nodes[[1]]$task, " {package} ")
  to_report <- mapply(
    function(label, i) {
      x <- report_task_ansi_tty(reporter = reporter, g = g, v = V(g)[[i]])
      x$label <- label
      x
    },
    package_task_labels,
    rev_dep_check_nodes,
    SIMPLIFY = FALSE
  )

  to_report <- append(
    to_report,
    after = 0L,
    list(reporter_line(
      style = "h1",
      label = reporter_cell(
        rev_dep,
        width = reporter$label_nchar,
        padding = c(2, 2)
      ),
      status = NA_character_
    ))
  )

  to_report
}

#' @export
report_task_ansi_tty.check_task <- function(reporter, g, v) {
  package <- fmt(task = v$task, "{package}")
  reporter_line(
    label = reporter_cell(
      package,
      justify = "right",
      width = reporter$label_nchar
    ),
    status = format_status_line_ansi(v$process)
  )
}

#' @export
report_sleep.reporter_ansi_tty <- function(
  reporter,
  checker,
  sleep = default_tty_tick_interval()
) {
  Sys.sleep(sleep)
}

#' @export
report_start_setup.reporter_ansi_tty <- function(
  reporter,
  checker,
  message,
  envir = parent.frame()
) {
  # hide cursor when initializer enters, ensure its restored even if interrupted
  cli::ansi_hide_cursor()
  do.call(
    on.exit,
    list(quote(cli::ansi_show_cursor()), add = TRUE),
    envir = envir
  )

  cli::cli_progress_message(
    message,
    clear = FALSE,
    auto_terminate = FALSE,
    .auto_close = FALSE,
    .envir = reporter,
  )
}

get_reporter_node <- function(g, v, mode = "out") {
  end <- if (mode == "out") igraph::head_of else igraph::tail_of
  v_es <- igraph::incident_edges(g, v, mode = mode)
  v_rep <- simplify2array(lapply(v_es, function(es) {
    eidx <- es$relation == RELATION$report
    as.numeric(end(g, es[eidx]))
  }))
  V(g)[ifelse(is.na(v_rep), as.numeric(v), v_rep)]
}

reporter_ansi_tty_get_label_nchar <- function(
  reporter,
  checker,
  envir = parent.frame()
) {
  # pre-calculate the maximum space needed for label column
  v <- igraph::V(checker$graph)
  v_report <- unique(get_reporter_node(checker$graph, v[is_check(v$task)]))

  # for each reporter, produce output
  output <- unlist(recursive = FALSE, lapply(
    v_report,
    function(v) report_task(reporter, checker$graph, V(checker$graph)[[v]])
  ))

  labels <- unlist(lapply(output, function(x) format(x$label, justify = NULL)))
  max(cli::ansi_nchar(labels))
}

#' @export
report_start_checks.reporter_ansi_tty <- function(
  reporter,
  checker,
  envir = parent.frame()
) {
  # store our current console width, used to trigger complete re-draw
  reporter$width <- cli::console_width()

  # derive the longest label, used for spacing table
  reporter$label_nchar <-
    reporter_ansi_tty_get_label_nchar(reporter, checker, envir)

  # helper function for adding new buffer entries
  reporter$buffer_proto <- function(node = "") {
    df <- data.frame(
      # tracking columns
      node = node, #   reporter node name
      new = TRUE,  #   whether node is newly added (requires newlines)
      updated = NA #   whether output needs to be updated
    )

    # output columns
    df$line <- list(NULL)
    df
  }

  # initialize our buffer of output
  reporter$buffer <- reporter$buffer_proto()[c(), ]

  reporter$buffer_update <- function(node, lines) {
    if (!node %in% reporter$buffer$node) {
      proto_df <- reporter$buffer_proto(node)[rep_len(1L, length(lines)), ]
      reporter$buffer <- rbind(reporter$buffer, proto_df)
    }

    is_node <- which(reporter$buffer$node == node)
    reporter$buffer$updated[is_node] <- TRUE
    reporter$buffer$line[is_node] <- lines
  }

  # helper to update buffer for a specific node
  reporter$buffer_report <- function(node) {
    node <- V(checker$graph)[node][[1]]
    to_report <- report_task(reporter, checker$graph, node)
    output <- vcapply(to_report, format)
    reporter$buffer_update(node, output)
  }

  # hide cursor when initializer enters, ensure its restored even if interrupted
  cli::ansi_hide_cursor()
  do.call(
    on.exit,
    list(quote(cli::ansi_show_cursor()), add = TRUE),
    envir = envir
  )

  cli::cli_progress_bar(
    type = "custom",
    extra = list(message = ""),
    format = paste(
      "ETA {cli::pb_eta}",
      "({cli::pb_current}/{cli::pb_total})",
      "[{cli::pb_elapsed}]",
      "{cli::pb_extra$message}"
    ),
    format_done = "Finished in {cli::pb_elapsed}",
    total = sum(igraph::V(checker$graph)$type == "check"),
    clear = FALSE,
    auto_terminate = FALSE,
    .auto_close = FALSE,
    .envir = reporter,
  )
}

#' @importFrom igraph V
#' @export
report_status.reporter_ansi_tty <- function(reporter, checker, envir) {
  v <- igraph::V(checker$graph)
  v_checks <- v[is_check(v$task)]

  # add newly started task status
  updated <- which(
    v_checks$status > STATUS$pending | (
      v_checks$status == STATUS$done &
        !v_checks$name %in% reporter$buffer$node
    )
  )

  # skip if no updates
  if (length(updated) <= 0L) {
    return()
  }

  # find any reporter nodes for updated checks
  updated <- unique(get_reporter_node(checker$graph, v_checks[updated]))

  # print header if this is the first status line of the reporter
  if (nrow(reporter$buffer) == 0L) {
    reporter$buffer_update(
      node = "HEADER",
      list(reporter_line(
        label = reporter_cell("", width = reporter$label_nchar),
        status = cli_table_row("S", "OK", "N", "W", "E", style = "title")
      ))
    )
  }

  for (node_name in updated$name) {
    reporter$buffer_report(node_name)
  }

  # if console width has changed, redraw all
  if (identical(reporter$width, cli::console_width)) {
    reporter$buffer$updated <- TRUE
  }

  # introduce newlines for newly added reporter rows
  buffer <- strrep("\n", sum(reporter$buffer$new))
  reporter$buffer$new <- FALSE

  # for each not-yet finished task, report status
  for (idx in which(reporter$buffer$updated)) {
    # derive reporter information
    n_lines <- nrow(reporter$buffer) - idx + 1L

    # report status line
    buffer <- paste0(
      buffer,
      ansi_move_line_rel(n_lines),
      ansi_line_erase(),
      format(reporter$buffer$line[[idx]]),
      ansi_move_line_rel(-n_lines),
      sep = ""
    )
  }

  reporter$width <- cli::console_width()
  reporter$buffer$updated <- FALSE
  cat(buffer)

  n_finished <- sum(v$status[v$type == "check"] >= STATUS$done)
  cli::cli_progress_update(
    set = n_finished,
    extra = list(message = ""),
    .envir = reporter
  )
}

#' @export
report_finalize.reporter_ansi_tty <- function(reporter, checker) {
  report_status(reporter, checker) # report completions of final processes
  cli::cli_progress_done(.envir = reporter)
  cli::ansi_show_cursor()
}
