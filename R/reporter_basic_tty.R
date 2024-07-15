#' @export
report_initialize.reporter_basic_tty <- function(
    # nolint
    reporter,
    design,
    envir = parent.frame()) {
  # start with initialized-as-completed tasks
  v <- igraph::V(design$graph)
  which_done <- v$status == STATUS$done
  done <- v[which_done]$status
  names(done) <- v$name[which_done]

  # named factor vector, names as task aliases and value of last reported status
  reporter$status <- done
  reporter$time_start <- Sys.time()

  cli::cli_text("<", utils::packageName(), "> Checks")
}

#' @export
report_status.reporter_basic_tty <- function(reporter, design, envir) { # nolint
  cli_theme()
  for (i in seq_along(igraph::V(design$graph))) {
    node <- igraph::V(design$graph)[[i]]

    # skip if queued, but not started
    if (node$status <= STATUS$`pending`) next

    # NOTE: for some reason check process never finishes unless we poll checks
    if (node$type == "check" && !is.null(node$process)) node$process$poll_output()

    # report stating of new checks
    if (!identical(node$status, reporter$statuses[[node$name]])) {
      status <- switch(as.character(node$status), # nolint
        "pending" = "queued",
        "in progress" = cli::cli_fmt(cli::cli_text("started")),
        "done" = {
          if (is.null(node$process)) {
            cli::cli_fmt(cli::cli_text("finished (restored)"))
          } else {
            dur <- if (!is.null(node$process$get_duration)) {
              node$process$get_duration()
            }
            if (node$type == "check") {
              ewn <- c("ERROR", "WARNING", "NOTE")
              ewn <- table(node$process$get_checks())[ewn]
            } else {
              ewn <- c(0, 0, 0)
            }
            cli::cli_fmt(cli::cli_text(
              "finished",
              if (sum(ewn) > 0) " with ",
              paste(collapse = ", ", c(
                if (ewn[[1]] > 0) cli::format_inline("{.err {ewn[[1]]} ERROR{?/S}}"),
                if (ewn[[2]] > 0) cli::format_inline("{.warn {ewn[[2]]} WARNING{?/S}}"),
                if (ewn[[3]] > 0) cli::format_inline("{.note {ewn[[3]]} NOTE{?/S}}")
              )),
              if (!is.null(dur)) cli::format_inline(" {.time_taken ({format_time(dur)})}")
            ))
          }
        }
      )

      time <- Sys.time() - reporter$time_start # nolint
      prefix <- cli::col_cyan("[{format_time(time)}][{node$type}] ")
      cli::cli_text(prefix, "{.pkg {node$name}} {status}")
      reporter$statuses[[node$name]] <- node$status
    }
  }
}

#' @export
report_finalize.reporter_basic_tty <- function(reporter, design) { # nolint
  cli_theme()
  report_status(reporter, design) # report completions of final processes
  time <- format_time(Sys.time() - reporter$time_start) # nolint
  cli::cli_text("Finished in {.time_taken {time}}")
}
