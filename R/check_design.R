#' Creating new Check Design Objects
#'
#' Instantiate a check design from a path or directory.
#'
#' @param x A file path, passed to [`rev_dep_check_tasks_df()`]
#' @param ... Additional arguments passed to [`new_check_design()`]
#'
#' @family checks
#' @export
new_check_design <- function(...) {
  check_design$new(...)
}

#' @family checks
#' @name new_check_design
#' @export
new_rev_dep_check_design <- function(x, ...) {
  tasks <- rev_dep_check_tasks_df(x)
  new_check_design(tasks, ...)
}

#' `R6` Checks Coordinator
#'
#' @description
#' A stateful object that orchestrates all separate processes required to
#' manage installation, library setup and run `R CMD check`s in sequence.
#'
#' @examples
#' \dontrun{
#' library(checked)
#' df <- source_check_tasks_df(c(
#'   system.file("example_packages", "exampleBad", package = "checked"),
#'   system.file("example_packages", "exampleGood", package = "checked")
#' ))
#'
#' plan <- check_design$new(df, n = 10, repos = "https://cran.r-project.org/")
#' while (!plan$is_done()) {
#'   plan$start_next_task()
#' }
#' }
#'
#' @family checks
#' @export
check_design <- R6::R6Class( # nolint cyclocomp_linter
  "check_design",
  public = list(
    #' @field graph (`igraph::igraph()`)\cr
    #' A dependency graph, storing information about which dependencies
    #' are required prior to execution of each check task.
    #' Created with [`task_graph_create()`]
    graph = NULL,

    #' @field input (`data.frame()`)\cr
    #' Checks task `data.frame` which is the source of all the checks.
    input = NULL,

    #' @field output (`character(1)`)\cr
    #' Output directory where raw results and temporary library will
    #' be created and stored.
    output = tempfile(paste(packageName(), Sys.Date(), "", sep = "-")),

    #' @description
    #' Initialize a new check design
    #'
    #' Use checks data.frame to generate task graph in which all dependencies
    #' and installation order are embedded.
    #'
    #' @param df `check_design` data.frame.
    #' @param n `integer` value indicating maximum number of subprocesses that
    #'    can be simultaneously spawned when executing tasks.
    #' @param output `character` value specifying path where the output should
    #'   be stored.
    #' @param lib.loc `character` vector with libraries allowed to be used when
    #'   checking packages, defaults to entire .libPaths().
    #' @param repos `character` vector of repositories which will be used when
    #'   generating task graph and later pulling dependencies.
    #' @param restore `logical` value, whether output directory should be
    #'   unlinked before running checks. If `FALSE`, an attempt will me made to
    #'   restore previous progress from the same `output`.
    #' @param ... Additional arguments unused
    #'
    #' @return [check_design].
    initialize = function(
      df,
      n = 2L,
      output = tempfile(paste(packageName(), Sys.Date(), sep = "-")),
      lib.loc = .libPaths(), # nolint object_name_linter
      repos = getOption("repos"),
      restore = options::opt("restore"),
      ...
    ) {
      # Make sure all aliases are unique
      stopifnot(
        "Check task aliases has to be unique" =
          !any(duplicated(df$alias)),
        "Check task aliases cannot have the same name as any of the available packages" = # nolint
          !any(df$alias %in% available.packages(repos = repos)[, "Package"]),
        "Custom package aliases cannot be duplicates of check aliases" =
          !any(uulist(drlapply(df$custom, `[[`, "alias")) %in% df$alias)
      )
      
      check_past_output(output, restore, ask = interactive())
      
      dir_create(output)
      
      self$input <- df
      self$output <- output
      private$n <- n
      private$lib.loc <- lib.loc
      private$repos <- repos

      g <- task_graph_create(df, repos)
      self$graph <- task_graph_update_done(g, c(path_lib(output), lib.loc))
      private$restore_complete_checks()
    },

    #' @description
    #' Get Active Processes list
    active_processes = function() {
      private$active
    },

    #' @description
    #' Get Failed Tasks list
    failed_tasks = function() {
      private$failed
    },

    #' @description
    #' Kill All Active Design Processes
    #'
    #' Immediately terminates all the active processes.
    terminate = function() {
      invisible(lapply(private$active, function(process) process$kill()))
    },

    #' @description
    #' Fill Available Processes with Tasks
    #'
    #' @return A logical value, indicating whether processes are actively
    #'   running.
    step = function() {
      while ((res <- self$start_next_task()) > 0) {}
      res >= 0
    },

    #' @description
    #' Start Next Task
    #'
    #' @return A integer value, coercible to logical to indicate whether a new
    #'   process was spawned, or `-1` if all tasks have finished.
    start_next_task = function() {
      # finish any finished processes
      for (process in private$active) {
        if (!process$is_alive()) {
          process$finish()
        } else if (inherits(process, "check_process")) {
          # NOTE: check process never finishes unless we poll checks
          process$poll_output()
        }
      }

      if (self$is_done()) {
        return(-1L)
      }
      
      # force garbage collection to free memory from terminated processes
      gc(verbose = FALSE, reset = FALSE, full = TRUE)

      # if all available processes are in use, terminate early
      n_active <- length(private$active)
      if (n_active >= private$n) {
        return(0L)
      }

      next_task <- task_graph_which_ready(self$graph)

      # If there are not new tasks cache, update the graph looking for new tasks
      if (length(next_task) == 0) {
        self$graph <- upadate_next_tasks(self$graph)
        next_task <- task_graph_which_ready(self$graph)
      }

      if (length(next_task) > 0) {
        next_task <- head(next_task, 1L)
        process <- start_task(
          task = next_task,
          g = self$graph,
          output = self$output,
          lib.loc = private$lib.loc
        )

        success <- private$push_process(next_task, process)
        return(as.integer(success))
      }

      finished <- (length(private$active) == 0) && self$is_done()
      return(-finished)
    },
    #' @description
    #' Check if checks are done
    #'
    #' Checks whether all the scheduled tasks were successfully executed.
    is_done = function() {
      checks <- igraph::V(self$graph)[igraph::V(self$graph)$type == "check"]
      all(checks$status == STATUS$done)
    }
  ),
  private = list(
    # Values

    # maximum child process count
    n = 2L,
    # lib.loc of allowed packages,
    lib.loc = NULL,
    # repositories to fetch dependencies from
    repos = getOption("repos"),
    # active processes
    active = list(),
    # failed tasks
    failed = list(),
    # Methods

    push_process = function(task, x) {
      task_graph_task_process(self$graph, task) <- x
      name <- task_graph_task_name(self$graph, task)
      task_graph_package_status(self$graph, task) <- STATUS$`in progress`
      x$set_finisher(function(process) {
        if (process$get_r_exit_status() != 0) {
          private$failed[[name]] <- task
        }
        private$pop_process(name)
        task_graph_package_status(self$graph, task) <- STATUS$done
      })
      private$active[[name]] <- x
      TRUE
    },
    restore_complete_checks = function() {
      checks <- self$input$alias
      check_done <- vlapply(checks, function(check) {
        file.exists(file.path(
          path_check_output(self$output, check),
          "result.json"
        ))
      })
      self$graph <- task_graph_set_package_status(
        self$graph,
        checks[check_done],
        STATUS$done
      )
    },
    pop_process = function(name) {
      private$active[[name]] <- NULL
    }
  )
)

#' @export
print.check_design <- function(x, ...) {
  if (x$is_done()) {
    print(results(x, ...), ...)
  } else {
    print(x$input, ...)
  }
  invisible(x)
}

check_past_output <- function(output, restore, ask = interactive()) {
  if (dir.exists(output)) {
    if (is.na(restore)) {
      restore <- if (ask) {
        switch(
          restore_menu(),
          "1" = TRUE,
          "2" = FALSE
        )
      } else {
        FALSE
      }
    }
    
    if (!restore) {
      unlink(output, recursive = TRUE, force = TRUE)
    }
  }
}

restore_menu <- function() {
  utils::menu(
    c("Yes", "No"),
    title = "Do you want to restore previous results?"
  )
}
