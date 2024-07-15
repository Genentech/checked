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
report_sleep.reporter_ansi_tty <- function(
    reporter,
    design,
    sleep = default_tty_tick_interval()) {
  Sys.sleep(sleep)
}

#' @export
report_initialize.reporter_ansi_tty <- function(
    reporter,
    design,
    envir = parent.frame()) {
  # named factor vector, names as task aliases and value of last reported status
  reporter$header <- TRUE
  reporter$status <- STATUS$done[c()]

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
    format = "ETA {cli::pb_eta} ({cli::pb_current}/{cli::pb_total}) [{cli::pb_elapsed}] {cli::pb_extra$message}", # nolint
    format_done = "Finished in {cli::pb_elapsed}",
    total = sum(igraph::V(design$graph)$type == "check"),
    clear = FALSE,
    auto_terminate = TRUE,
    .envir = reporter,
  )
}

#' @importFrom igraph V
#' @export
report_status.reporter_ansi_tty <- function(reporter, design, envir) { # nolint
  v <- igraph::V(design$graph)
  v_checks <- v[v$type == "check"]
  n_char_titles <- max(nchar(v_checks$name))

  # add newly started task status
  new_idx <- which(v_checks$status > STATUS$pending)
  new_idx <- new_idx[!v_checks$name[new_idx] %in% names(reporter$status)]
  if (length(new_idx) > 0) {
    # print header if this is the first status line of the reporter
    if (reporter$header) {
      cat(
        ansi_line_erase(),
        strrep(" ", n_char_titles + 2),
        cli_table_row("S", "OK", "N", "W", "E", title = TRUE),
        "\n",
        sep = ""
      )
      reporter$header <- FALSE
    }

    # always start by reporting in progress, even if finished before reporting
    new <- rep_len(STATUS$`in progress`, length(new_idx))
    names(new) <- v_checks$name[new_idx]
    reporter$status <- c(reporter$status, new)
    cat(strrep("\n", length(new_idx)))
  }

  # for each not-yet finished task, report status
  buffer <- ""
  for (idx in which(reporter$status < STATUS$done)) {
    # update reported status
    alias <- names(reporter$status)[[idx]]
    v_idx <- which(v_checks$name == alias)
    reporter$status[[idx]] <- v_checks$status[[v_idx]]

    # derive reporter information
    n_lines <- length(reporter$status) - idx + 1L
    width <- cli::console_width() - n_char_titles - 2L
    task_name <- v_checks$name[[v_idx]]
    process <- task_graph_task_process(design$graph, v_checks[[v_idx]])
    # If the check was restored it does not have process associated with it.
    if (is.null(process)) next()

    # report status line
    buffer <- paste0(
      buffer,
      ansi_move_line_rel(n_lines),
      ansi_line_erase(),
      " ", strrep(" ", n_char_titles - nchar(task_name)), task_name, " ",
      format_status_line_ansi(process, width = width),
      ansi_move_line_rel(-n_lines),
      sep = ""
    )
  }

  cat(buffer)

  is_inst <- vlapply(design$active_processes(), inherits, "install_package_process") # nolint
  inst_pkgs <- names(design$active_processes()[is_inst])
  if (length(inst_pkgs)) {
    inst_msg <- paste0("installing ", paste0(inst_pkgs, collapse = ", "))
  } else {
    inst_msg <- ""
  }

  n_finished <- sum(v$status[v$type == "check"] >= STATUS$done)
  cli::cli_progress_update(
    set = n_finished,
    extra = list(message = inst_msg),
    .envir = reporter
  )
}

#' @export
report_finalize.reporter_ansi_tty <- function(reporter, design) { # nolint
  report_status(reporter, design) # report completions of final processes
  cli::ansi_show_cursor()
}
