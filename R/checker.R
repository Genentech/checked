#' Creating new Check Design Objects
#'
#' Instantiate a check design from a path or directory.
#'
#' @param x A file path, passed to [`plan_rev_dep_checks()`]
#' @param ... Additional arguments passed to [`new_checker()`]
#'
#' @family checks
#' @export
new_checker <- function(...) {
  checker$new(...)
}

#' @family checks
#' @name new_checker
#' @export
new_rev_dep_checker <- function(x, ...) {
  plan <- plan_rev_dep_checks(x)
  new_checker(plan, ...)
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
#' plan <- plan_checks(c(
#'   system.file("example_packages", "exampleBad", package = "checked"),
#'   system.file("example_packages", "exampleGood", package = "checked")
#' ))
#'
#' orchestrator <- checker$new(
#'   plan,
#'   n = 10,
#'   repos = "https://cran.r-project.org/"
#' )
#'
#' while (!orchestrator$is_done()) {
#'   orchestrator$start_next_task()
#' }
#' }
#'
#' @family checks
#' @export
checker <- R6::R6Class(
  "checker",
  public = list(
    #' @field graph (`igraph::igraph()`)\cr
    #' A dependency graph, storing information about which dependencies
    #' are required prior to execution of each check task.
    #' Created with [`task_graph()`]
    graph = NULL,

    #' @field plan (`data.frame()`)\cr
    #' Checks task `data.frame` which is the source of all the checks.
    plan = NULL,

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
    #' @param plan `plan` `data.frame`.
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
    #' @return [checker].
    initialize = function(
      plan,
      n = 2L,
      output = file.path(
        tempdir(),
        paste(packageName(), Sys.Date(), sep = "-")
      ),
      lib.loc = .libPaths(),
      repos = getOption("repos"),
      restore = options::opt("restore"),
      ...
    ) {
      check_past_output(output, restore, ask = interactive())

      dir_create(output)

      self$plan <- plan
      self$output <- output
      private$n <- n
      private$lib.loc <- c(
        # Append checker designated library
        path_checker_lib(output),
        lib.loc
      )
      private$repos <- repos

      self$graph <- task_graph(self$plan, repos)
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
      for (process in private$active) {
        if (!process$is_alive()) {
          process$finish()
          private$gc_needed <- TRUE
        } else if (inherits(process, "check_process")) {
          # NOTE: check process never finishes unless we poll checks
          process$poll_output()
        }
      }

      if (self$is_done()) {
        return(-1L)
      }

      if (options::opt("proactive_gc") && private$gc_needed) {
        gc(verbose = FALSE, reset = FALSE, full = TRUE)
        private$gc_needed <- FALSE
      }

      # if all available processes are in use, terminate early
      n_active <- length(private$active)
      if (n_active >= private$n) {
        return(0L)
      }

      next_node <- private$get_next_node()

      if (length(next_node) != 0) {
        process <- start_task(
          node = next_node,
          g = self$graph,
          output = self$output,
          lib.loc = private$lib.loc
        )

        if (is.null(process)) {
          private$finish_node(next_node)
          return(1L)
        }

        success <- private$push_process(next_node, process)
        return(as.integer(success))
      }

      if (length(private$active) == 0 && !self$is_done()) {
        stop("some tasks not executed due to unsolved dependency requirements")
      }

      finished <- (length(private$active) == 0) && self$is_done()

      -finished
    },

    #' @description
    #' Check if checks are done
    #'
    #' Checks whether all the scheduled tasks were successfully executed.
    is_done = function() {
      is_check_node <- is_check(V(self$graph)$task)
      checks <- V(self$graph)[is_check_node]
      if (length(checks) > 0) {
        all(checks$status == STATUS$done)
      } else {
        # If there is not a single check tasks, we need to make sure all
        # the task finished
        all(V(self$graph)$status == STATUS$done)
      }

    }
  ),
  private = list(
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

    # task loop counter
    gc_needed = FALSE,

    start_node = function(node) {
      task_graph_package_status(self$graph, node) <- STATUS$`in progress`
      private$start_node_meta_parents(node)
    },

    start_node_meta_parents = function(node) {
      # start parent meta tasks recursively once a child starts
      parent_nodes <- igraph::adjacent_vertices(self$graph, node, mode = "in")
      parent_nodes <- unlist(parent_nodes, use.names = FALSE)
      parent_nodes <- V(self$graph)[parent_nodes]
      meta_parent_nodes <- parent_nodes[is_meta(parent_nodes$task)]
      for (meta in meta_parent_nodes) {
        task_graph_package_status(self$graph, meta) <- STATUS$`in progress`
        private$start_node_meta_parents(meta)
      }
    },

    finish_node = function(task) {
      task_graph_package_status(self$graph, task) <- STATUS$`done`
      private$finish_node_meta_parents(task)
    },

    finish_node_meta_parents = function(node) {
      # parent meta tasks finish once _all_ children are finished
      parent_nodes <- igraph::adjacent_vertices(self$graph, node, mode = "in")
      parent_nodes <- unlist(parent_nodes, use.names = FALSE)
      parent_nodes <- V(self$graph)[parent_nodes]
      meta_parent_nodes <- parent_nodes[is_meta(parent_nodes$task)]
      for (meta in meta_parent_nodes) {
        siblings_by_meta_parent <-
          igraph::adjacent_vertices(self$graph, meta, "out")
        for (siblings in siblings_by_meta_parent) {
          if (!all(siblings$status == STATUS$`done`)) next
          task_graph_package_status(self$graph, meta) <- STATUS$`done`
        }
      }
    },

    push_process = function(node, x) {
      task_graph_task_process(self$graph, node) <- x
      name <- task_graph_task_name(self$graph, node)
      private$start_node(node)
      x$set_finisher(function(process) {
        if (process$get_r_exit_status() != 0) {
          private$failed[[name]] <- node
        }
        private$pop_process(name)
        private$finish_node(node)
      })
      private$active[[name]] <- x
      TRUE
    },

    restore_complete_checks = function() {
      checks <- V(self$graph)[is_check(V(self$graph)$task)]
      check_done <- vlapply(checks$name, function(check) {
        file.exists(file.path(
          path_check_output(self$output, check),
          "result.json"
        ))
      })

      for (task in checks[check_done]) {
        private$finish_node(task)
      }
    },

    pop_process = function(name) {
      private$active[[name]] <- NULL
    },
    # With these approach we deviate slightly from always prioritizing check
    # tasks across the entire graph in favor of prioritizing them on the given
    # layer. We do that due to significant performance gain in graph searching.
    get_next_node = function(force = FALSE) {
      ready_nodes <- task_graph_which_ready(self$graph)
      if (length(ready_nodes) == 0 || force) {
        self$graph <- task_graph_update_check_ready(self$graph)
        self$graph <- task_graph_update_install_ready(self$graph)
        ready_nodes <- task_graph_which_ready(self$graph)
      }
      utils::head(ready_nodes, 1L)
    }
  )
)

#' @export
print.checker <- function(x, ...) {
  if (x$is_done()) {
    print(results(x, ...), ...)
  } else {
    print(x$plan, ...)
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
