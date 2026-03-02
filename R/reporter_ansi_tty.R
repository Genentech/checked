#' @export
#' @method report_task reporter_ansi_tty
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
    status,
    width = getOption("width", 80L)) {

  if (status == STATUS$done) {
    msg <- "restored from system file."
    status <- "NONE"
  } else {
    msg <- "starting ..."
    status <- silent_spinner(list(
      frames = c("\u2834", "\u2826", "\u2816", "\u2832")
    ))$spin()
  }

  out <- cli_table_row(
    status = status,
    ok = 0,
    notes = 0,
    warnings = 0,
    errors = 0,
    msg
  )

  cli::ansi_substring(out, 1, width)
}

reporter_line <- function(label, status, style = NA_character_) {
  structure(
    list(label = label, status = status, style = style),
    class = "reporter_line"
  )
}

#' @export
format.reporter_line <- function(x, width = cli::console_width(), ...) {
  rule <- switch(
    x$style,
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
format.reporter_cell <- function(
  x,
  padding = attr(x, "padding") %||% c(0, 0),
  justify = attr(x, "justify") %||% "right",
  width = attr(x, "width") %||% cli::console_width(),
  pad = " ",
  ...
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
report_task_ansi_tty.default <- function(reporter, g, v) {
  NULL
}

#' @export
report_task_ansi_tty.rev_dep_check_meta_task <- function(reporter, g, v) {
  # package being rev-dep-check'ed
  package <- v$task$origin$package

  # get individual rev dep checks, and get their trees
  check_nodes <- igraph::neighbors(g, v$name, "out")

  # NOTE: do we only want to check _direct_ reverse dependencies?
  check_neighborhoods <- igraph::make_neighborhood_graph(
    g,
    order = 1,
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

  rev_dep <- paste0(" ", v$task$revdep, " ")
  to_report <- mapply(
    function(label, i) {
      x <- report_task_ansi_tty(reporter = reporter, g = g, v = V(g)[[i]])
      x$label <- label
      x
    },
    package_task_labels,
    check_nodes,
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
    status = format_status_line_ansi(v$process, status = v$status)
  )
}

#' @export
report_sleep.reporter_ansi_tty <- function(
  reporter,
  checker,
  sleep = options::opt("tty_tick_interval")
) {
  Sys.sleep(sleep)
}

#' @export
report_start_setup.reporter_ansi_tty <- function(
  reporter,
  checker,
  ...,
  envir = parent.frame()
) {
  if (cli_env_has_pb(reporter$cli)) return()
  reporter$cli <- new.env(parent = reporter)

  reporter$tty_height <- ansi_tty_height()
  # Buffer is shorter than actual tty by one line due to progress bar existance
  reporter$buffer_height <- reporter$tty_height - 1

  # hide cursor when initializer enters, ensure its restored even if interrupted
  cli::ansi_hide_cursor()
  do.call(
    on.exit,
    list(quote(cli::ansi_show_cursor()), add = TRUE),
    envir = envir
  )

  cli::cli_progress_bar(
    type = "custom",
    format = paste(
      "ETA {cli::pb_eta}",
      "({cli::pb_current}/{cli::pb_total})",
      "[{cli::pb_elapsed}]",
      "{cli::pb_extra$message}"
    ),
    format_done = "Finished in {cli::pb_elapsed}",
    ...,
    clear = FALSE,
    auto_terminate = FALSE,
    .auto_close = FALSE,
    .envir = reporter$cli
  )

  cli::cli_progress_update(force = TRUE, .envir = reporter$cli)
}

reporter_ansi_tty_get_label_nchar <- function(
  reporter,
  checker,
  envir = parent.frame()
) {
  # pre-calculate the maximum space needed for label column
  v <- igraph::V(checker$graph)
  v_report <- v[is_meta(v$task)]

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
  ...,
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
      node = node,  #   reporter node name
      new = TRUE,   #   whether node is newly added (requires newlines)
      updated = NA, #   whether output needs to be updated
      final = FALSE #   whether element is no longer a subject to change
    )

    # output columns
    df$line <- list(NULL)
    df
  }

  # initialize our buffer of output
  reporter$buffer <- reporter$buffer_proto()[c(), ]

  reporter$buffer_update <- function(node, lines, final = FALSE) {
    if (!node %in% reporter$buffer$node) {
      proto_df <- reporter$buffer_proto(node)[rep_len(1L, length(lines)), ]
      reporter$buffer <- rbind(reporter$buffer, proto_df)
    }

    is_node <- which(reporter$buffer$node == node)
    reporter$buffer$updated[is_node] <- TRUE
    reporter$buffer$final[is_node] <- final
    reporter$buffer$line[is_node] <- lines
  }

  # helper to update buffer for a specific node
  reporter$buffer_report <- function(node) {
    node <- V(checker$graph)[node][[1]]
    to_report <- report_task(reporter, checker$graph, node)
    output <- vcapply(to_report, format)
    final <- node$status == STATUS$done
    reporter$buffer_update(node, output, final = final)
  }

  extra <- list(message = "")
  if (cli_env_has_pb(reporter$cli)) {
    cli::cli_progress_update(extra = extra, .envir = reporter$cli)
  } else {
    report_start_setup(
      reporter,
      checker,
      extra = extra,
      total = sum(is_check(igraph::V(checker$graph)$task)),
      envir = envir
    )
  }
}

#' @importFrom igraph V
#' @export
report_status.reporter_ansi_tty <- function(reporter, checker, envir) {
  msg <- c()
  v <- igraph::V(checker$graph)

  # add newly started task status
  is_running <- v$status > STATUS$ready & v$status < STATUS$done

  is_done <- v$status >= STATUS$done
  is_non_final_in_buffer <- vlapply(v, function(n) {
    if (n %in% reporter$buffer$node) {
      reporter$buffer$final[which(reporter$buffer$node == n)[[1]]]
    } else {
      FALSE
    }
  })
  is_newly_done <- is_done & !is_non_final_in_buffer
  updated <- v[is_running | is_newly_done]

  # skip if no updates
  if (length(updated) <= 0L) {
    return()
  }

  # print header if this is the first status line of the reporter
  if (nrow(reporter$buffer) == 0L) {
    reporter$buffer_update(
      node = "HEADER",
      list(reporter_line(
        label = reporter_cell("", width = reporter$label_nchar),
        status = cli_table_row("S", "OK", "N", "W", "E", style = "title")
      )),
      final = TRUE
    )
  }

  # report check tasks in cli output
  to_report_main <- is_meta(updated$task)
  for (node_name in updated[to_report_main]$name) {
    reporter$buffer_report(node_name)
  }

  # report non-check tasks in status line
  to_report_bar <- is_install(updated$task) &
    updated$status > STATUS$ready &
    updated$status < STATUS$done

  if (any(to_report_bar)) msg[length(msg) + 1L] <- "installing"
  for (node_name in updated[to_report_bar]$name) {
    msg[length(msg) + 1L] <- fmt("{package}", task = updated[[node_name]]$task)
  }

  # if console width has changed, redraw all
  if (identical(reporter$width, cli::console_width)) {
    reporter$buffer$updated <- TRUE
  }

  # Get number of lines that are final and didn't update since the last pass
  finalized_lines <-
    cumprod(!reporter$buffer$updated & reporter$buffer$final) == 1
  # Finalized lines as well as lines that are in progress but would exceed
  # the maximum height of the tty should be skipped from reporting until
  # the number of finished tasks allows to fit new ones.
  reportable_lines <- !finalized_lines
  # Which max returns the position of first TRUE, then we allow folloiwng
  # reporter$buffer_height lines to be reported at the time
  if (sum(reportable_lines) > reporter$buffer_height) {
    reportable_lines[seq(which.max(reportable_lines) + reporter$buffer_height,
                         length(reportable_lines))] <- FALSE
  }

  # introduce newlines for newly added reporter rows
  buffer <- strrep(
    "\n", sum(reporter$buffer$new & reportable_lines)
  )
  reporter$buffer$new[reportable_lines] <- FALSE

  # for each not-yet finished task, report status
  for (idx in which(reporter$buffer$updated & reportable_lines)) {
    # derive reporter information
    n_lines <- sum(reportable_lines) - idx + 1L + sum(finalized_lines)

    buffer <- paste0(
      buffer,
      ansi_move_line_rel(n_lines),
      ansi_line_erase(),
      format(reporter$buffer$line[[idx]]),
      ansi_move_line_rel(-n_lines),
      sep = ""
    )

    reporter$buffer$updated[idx] <- FALSE
  }

  reporter$width <- cli::console_width()
  cat(buffer)

  n_finished <- sum(v[is_check(v$task)]$status >= STATUS$done)
  cli::cli_progress_update(
    set = n_finished,
    extra = list(message = paste(msg, collapse = " ")),
    .envir = reporter$cli
  )
}

#' @export
report_finalize.reporter_ansi_tty <- function(reporter, checker) {
  # Init buffer
  report_status(reporter, checker)
  # Iterate until buffer is fully processed from final results
  while (!all(!reporter$buffer$updated & reporter$buffer$final)) {
    report_status(reporter, checker)
  }
  cli::cli_progress_done(.envir = reporter$cli)
  cat(ansi_line_erase()) # clear lingering progress bar output

  # Display failures
  failed_tasks <- checker$failed_tasks()
  failed_packages <-
    failed_tasks[vlapply(failed_tasks, function(x) is_install(x$task))]

  failures_output <- vcapply(failed_packages, function(x) {
    msg <- paste0(
      cli_wrap_lines(cli::cli_fmt(cli::cli_alert_danger(
        sprintf(
          "%s package installation had non-zero exit status",
          package(x$task[[1]])
        )
      ))),
      collapse = "\n"
    )
    log <- paste0(
      cli_wrap_lines(cli::cli_fmt(cli::cli_alert_danger(
        sprintf("log: %s", x$process[[1]]$log)
      ))),
      collapse = "\n"
    )
    paste0(
      msg, "\n", log
    )
  })

  cat(failures_output, "\n")
  cli::ansi_show_cursor()
}

#' @export
report_step.reporter_ansi_tty <- function(reporter, checker) {
  checker$start_next_task() >= 0
}
