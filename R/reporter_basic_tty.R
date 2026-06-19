#' @export
report_start_setup.reporter_basic_tty <- function(
  reporter,
  checker,
  ...,
  envir = parent.frame()
) {
  # start with all tasks initialized as pending
  v <- igraph::V(checker$graph)
  v_reportable <- if (reporter$checks_only) {
    v[is_check(v$task)]
  } else {
    v[is_actionable_task(v$task)]
  }

  # named factor vector, names as task aliases and value of last reported status
  reporter$status <- rep(STATUS$pending, times = length(v_reportable))
  names(reporter$status) <- v_reportable$name

  reporter$time_start <- Sys.time()

  type <- if (all(vlapply(v_reportable$task, inherits, "install_task"))) {
    "> Installs"
  } else {
    "> Checks"
  }

  cli::cli_text("<", utils::packageName(), type)
}

#' @export
report_status.reporter_basic_tty <- function(reporter, checker, envir) {
  cli_theme()
  g <- checker$graph
  tasks_names <- names(reporter$status)
  reported_done <- names(reporter$status[reporter$status == STATUS$done])
  tasks_not_started <-
    names(igraph::V(g)[igraph::V(g)$status <= STATUS$`ready`])
  tasks_to_report <-
    tasks_names[!tasks_names %in% c(reported_done, tasks_not_started)]

  for (i in igraph::V(g)[tasks_to_report]) {
    node <- igraph::V(g)[[i]]

    # skip if queued, but not started
    if (node$status <= STATUS$`pending`) next

    p <- node$process

    failure_message <- NULL
    # report stating of new checks
    if (!identical(node$status, reporter$status[[node$name]])) {
      status <- switch( # nolint
        as.character(node$status), # nolint (used via glue)
        "pending" = "queued",
        "in progress" = cli::cli_fmt(cli::cli_text("started")),
        "done" = {
          pkgs_done <- reporter$status == STATUS$done # nolint
          # +1 to acount for the task that is currently processed
          fmt_count <-
            cli::col_grey(" [{sum(pkgs_done)+1}/{length(pkgs_done)}]")

          if (is.null(p)) {
            cli::cli_fmt(
              cli::cli_text(
                "finished", cli::format_inline(fmt_count), " (restored)"
              )
            )
          } else if (p$get_r_exit_status() != 0) {
            # checks processes don't have logs associated with it
            failure_message <- if (inherits(p, "install_process")) {
              paste(
                p$get_results_safe(),
                if (file.exists(p$log)) sprintf("\nfull log: '%s'", p$log),
                sep = "\n",
                collapse = "\n"
              )
            }
            cli::cli_fmt(
              cli::cli_text(
                "failed", cli::format_inline(fmt_count)
              )
            )
          } else {
            dur <- if (!is.null(p$get_duration)) {
              p$get_duration()
            }

            if (is_check(node$task)) {
              ewn <- c("ERROR", "WARNING", "NOTE")
              ewn <- table(p$get_checks())[ewn]
            } else {
              ewn <- c(0, 0, 0)
            }

            fmt_error <- "{.err {ewn[[1]]} ERROR{?/S}}"
            fmt_warning <- "{.warn {ewn[[2]]} WARNING{?/S}}"
            fmt_note <- "{.note {ewn[[3]]} NOTE{?/S}}"
            fmt_duration <- " {.time_taken ({format_time(dur)})}"

            cli::cli_fmt(cli::cli_text(
              "finished",
              if (sum(ewn) > 0) " with ",
              paste(collapse = ", ", c(
                if (ewn[[1]] > 0) cli::format_inline(fmt_error),
                if (ewn[[2]] > 0) cli::format_inline(fmt_warning),
                if (ewn[[3]] > 0) cli::format_inline(fmt_note)
              )),
              cli::format_inline(fmt_count),
              if (!is.null(dur)) cli::format_inline(fmt_duration)
            ))
          }
        }
      )

      time <- Sys.time() - reporter$time_start # nolint (used via glue)
      type <- format_task_type(node$task) # nolint (used via glue)
      prefix <- cli::col_cyan("[{format_time(time)}][{type}] ")
      cli::cli_text(prefix, "{.pkg {package(node$task)}} {status}")
      if (!is.null(failure_message)) cli::cli_text(failure_message)
      reporter$status[[node$name]] <- node$status
    }
  }
}

#' @export
report_finalize.reporter_basic_tty <- function(reporter, checker) {
  cli_theme()
  report_status(reporter, checker) # report completions of final processes
  time <- format_time(Sys.time() - reporter$time_start) # nolint (used via glue)
  cli::cli_text("Finished in {.time_taken {time}}")
}

#' @export
report_sleep.reporter_basic_tty <- function(
  reporter,
  checker,
  sleep = 0
) {
  # Basci tty does not need sleep
  NULL
}

#' @export
report_step.reporter_basic_tty <- function(reporter, checker) {
  checker$start_next_task() >= 0
}
