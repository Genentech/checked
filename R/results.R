CHECK_ISSUES_TYPES <- c("notes", "warnings", "errors")

#' Check results
#'
#' Get R CMD check results
#'
#' @param x object which results should be presented.
#' @param checker_obj [`checker`] object.
#' @eval options::as_params("error_on" = "results_error_on")
#' @param ... other parameters.
#'
#' @family results
#' @export
results <- function(x, ...) {
  UseMethod("results")
}

#' @export
#' @rdname results
results.checker <- function(
    x,
    error_on = options::opt("results_error_on"),
    ...) {
  error_on <- match.arg(error_on, c("never", "issues", "potential_issues"))

  if (!x$is_done()) {
    warning("checker object does not represnet a completed run. Presenting partial results") # nolint
  }

  vs <- V(x$graph)
  # Find root's of meta nodes
  meta_nodes <- vs[is_meta(vs$task)]
  root_meta_nodes_idx <- meta_nodes[
    igraph::ego_size(x$graph, nodes = meta_nodes, mode = "in") == 1
  ]

  res <- structure(
    lapply(root_meta_nodes_idx, results, checker_obj = x),
    names = vs[root_meta_nodes_idx]$name,
    class = "checked_results"
  )

  if (error_on != "never") {
    potential_errors <- vlapply(res, function(y) {
      df <- results_to_df(y, issues_type = error_on)
      any(rowSums(df) != 0)
    })

    if (any(potential_errors)) {
      print(res)
      stop("Issues identified. Aborting.")
    }
  }

  res
}

#' @export
#' @rdname results
results.integer <- function(x, checker_obj, ...) {
  results(V(checker_obj$graph)[[x]], checker_obj = checker_obj)
}

#' @export
#' @method results igraph.vs
#' @rdname results
results.igraph.vs <- function(x, ...) {
  UseMethod("results", structure(0L, class = class(x$task)))
}

#' @export
#' @rdname results
results.rev_dep_dep_meta_task <- function(x, checker_obj, ...) {
  # x is a igraph.vs with rev_dep_dep_meta_task task
  nh <- igraph::neighbors(checker_obj$graph, x, mode = "out")
  nh <- nh[nh$status == STATUS$done]
  structure(
    lapply(nh, results, checker_obj = checker_obj),
    names = nh$name,
    package = package(x$task),
    class = "rev_dep_dep_results"
  )
}

#' @export
#' @rdname results
results.rev_dep_check_meta_task <- function(x, checker_obj, ...) {
  # x is a igraph.vs with rev_dep_dep_meta_task task
  checks <- igraph::neighbors(checker_obj$graph, x, mode = "out")
  seeds <- vcapply(checks, function(v) {
    V(checker_obj$graph)[[v]]$task$seed
  })
  # seeds are dev and release, sort alphanumerical to make sure dev is always
  # at the first index
  checks <- checks[order(seeds)]

  results_revdep_check(
    dev = checks[[1]]$name,
    release = checks[[2]]$name,
    output = checker_obj$output
  )
}

#' @export
#' @rdname results
results.local_check_meta_task <- function(x, checker_obj, ...) {
  # x is a igraph.vs with rev_dep_dep_meta_task task
  nh <- igraph::neighbors(checker_obj$graph, x, mode = "out")
  structure(
    lapply(nh$name, results_check, output = checker_obj$output),
    names = nh$name,
    class = "local_check_results"
  )
}

results_revdep_check <- function(dev, release, output = NULL, ...) {
  # Make it work either with full path or name of the package
  dev_path <- if (is.null(output) || file.exists(dev)) {
    dev
  } else {
    file.path(path_check_output(output, dev), "result.json")
  }

  release_path <- if (is.null(output) || file.exists(release)) {
    release
  } else {
    file.path(path_check_output(output, release), "result.json")
  }
  dev_check <- rcmdcheck_from_json(dev_path)
  release_check <- rcmdcheck_from_json(release_path)

  structure(
    lapply(CHECK_ISSUES_TYPES, function(i) {
      dev_check_i <- structure(
        # If no issues identified, object is an empty list instead of
        # a character vector. Changing it to empty character for consistency.
        if (is.list(dev_check[[i]])) character(0) else dev_check[[i]],
        names = get_issue_header(dev_check[[i]])
      )
      release_check_i <- structure(
        if (is.list(release_check[[i]])) character(0) else release_check[[i]],
        names = get_issue_header(release_check[[i]])
      )

      matching_headers_idx <- names(dev_check_i) %in% names(release_check_i)
      # Create temporary object with "See <path> for details" path
      # stripped out as well as all whitespaces. As they will always emit
      # potential issues due to the path or screen differences
      dev_check_i_tmp <- strip_details_from_issue(dev_check_i)
      release_check_i_tmp <- strip_details_from_issue(release_check_i)
      matching_messages_idx <- dev_check_i_tmp %in% release_check_i_tmp

      new_issues <- structure(
        unname(dev_check_i[!matching_headers_idx]),
        class = "issues"
      )
      new_potential_issues <- dev_check_i[
        matching_headers_idx & !matching_messages_idx
      ]
      new_potential_issues <- structure(
        list(
          new = unname(new_potential_issues),
          old = unname(release_check_i[names(new_potential_issues)])
        ),
        class = "potential_issues"
      )

      list("issues" = new_issues, "potential_issues" = new_potential_issues)
    }),
    names = CHECK_ISSUES_TYPES,
    package = dev_check$package,
    class = c("rcmdcheck_rev_dep_results", "rcmdcheck_results")
  )
}

results_check <- function(x, output, ...) {
  # Make it work either with full path or name of the package
  x_path <- if (is.null(output) || file.exists(x)) {
    x
  } else {
    file.path(path_check_output(output, x), "result.json")
  }

  x_check <- rcmdcheck_from_json(x_path)

  structure(
    lapply(CHECK_ISSUES_TYPES, function(i) {
      x_check_i <- x_check[[i]]

      new_issues <- structure(
        unname(x_check_i),
        class = "issues"
      )

      list("issues" = new_issues)
    }),
    names = CHECK_ISSUES_TYPES,
    package = x_check$package,
    class = c("rcmdcheck_check_results", "rcmdcheck_results")
  )
}


#' Summarize checked results as data.frame
#'
#' Turns checked results into a list of data.frams, one for each meta root task.
#' Such form makes quick results analysis easier by providing a general overview
#' of identified failures.
#'
#' @param results checked_results object or any of the sub-objects.
#' @param ... other prameters passed to downstream functions.
#' @export
#'
#' @family results
results_to_df <- function(results, ...) {
  UseMethod("results_to_df")
}


#' @export
results_to_df.checked_results <- function(results, ...) {
  lapply(results, results_to_df)
}


#' @export
results_to_df.default <- function(results, ...) {
  if (length(results) == 0) {
    data.frame(
      notes = character(0),
      warnings = character(0),
      errors = character(0),
      row.names = names(results)
    )
  } else {
    data.frame(
      notes = vnapply(results, count, type = "notes", ...),
      warnings = vnapply(results, count, type = "warnings", ...),
      errors = vnapply(results, count, type = "errors", ...),
      row.names = names(results)
    )
  }
}

filter_results <- function(x, keep, ...) {
  keep <- match.arg(keep, c("all", "issues", "potential_issues"))

  if (keep != "all") {
    df <- results_to_df(x, issues_type = keep)
    issues <- rowSums(df) != 0
    x[issues]
  } else {
    x
  }
}

count <- function(d, ...) {
  UseMethod("count")
}

#' @export
count.default <- function(d, type, ...) {
  sum(vnapply(d[[type]], count, ...))
}

#' @export
count.issues <- function(d, ...) {
  length(d)
}

#' @export
count.potential_issues <- function(d, issues_type = "potential_issues", ...) {
  if (issues_type == "issues") 0 else length(d$new)
}

#' Print checked results
#'
#' @param x an object to be printed.
#' @eval options::as_params("keep" = "results_keep")
#' @param name character name of the `rev_dep_dep` package
#' @param ... other parameters.
#'
#' @family results
#' @export
print.checked_results <- function(x, ...) {
  for (i in seq_along(x)) {
    print(x[[i]], ..., name = names(x)[[i]])
    cat("\n\n\n")
  }
  invisible(x)
}

#' @name print.checked_results
#' @export
print.rev_dep_dep_results <- function(
  x,
  ...,
  name = NULL,
  keep = options::opt("results_keep")
) {
  cat(sprintf(
    "# %s reverse dependency check results (%s) \n\n",
    attr(x, "package"),
    name
  ))

  x <- filter_results(x, keep = keep)

  for (i in seq_along(x)) {
    print(x[[i]], ...)
    cat("\n")
  }
  invisible(x)
}

#' @name print.checked_results
#' @export
print.local_check_results <- function(
  x,
  ...,
  name = NULL,
  keep = options::opt("results_keep")
) {
  cat(sprintf(
    "# Local check results (%s) \n\n",
    name
  ))

  x <- filter_results(x, keep = keep)

  for (i in seq_along(x)) {
    print(x[[i]], ...)
    cat("\n")
  }
  invisible(x)
}

get_issue_header <- function(x) {
  unname(vapply(x, function(y) {
    strsplit(y, "...", fixed = TRUE)[[1]][1]
  }, FUN.VALUE = character(1)))
}


rcmdcheck_to_json <- function(rcheck, file = NULL) {
  stopifnot(inherits(rcheck, "rcmdcheck"))

  json <- jsonlite::toJSON(
    unclass(rcheck),
    auto_unbox = TRUE,
    pretty = TRUE,
    force = TRUE # This is crucial to skip any environments in the rcheck object
  )

  if (!is.null(file)) {
    jsonlite::write_json(json, file, auto_unbox = TRUE)
  }

  json
}


rcmdcheck_from_json <- function(file) {
  stopifnot(file.exists(file))

  parsed <- jsonlite::fromJSON(file)
  structure(
    if (is.character(parsed)) jsonlite::fromJSON(parsed) else parsed,
    class = "rcmdcheck"
  )
}

#' @export
print.rcmdcheck_rev_dep_results <- function(x, ...) {
  cat(sprintf("%s package R CMD check diff \n", attr(x, "package")))
  NextMethod()
}

#' @export
print.rcmdcheck_check_results <- function(x, ...) {
  cat(sprintf("%s package R CMD check \n", attr(x, "package")))
  NextMethod()
}

#' @export
print.rcmdcheck_results <- function(x, ...) {
  for (i in CHECK_ISSUES_TYPES) {
    status <- if (length(x[[i]]$issues) > 0) {
      sprintf("NEW ISSUES [%s]", length(x[[i]]$issues))
    } else if (length(x[[i]]$potential_issues$new) > 0) {
      sprintf("NEW POTENTIAL ISSUES [%s]", length(x[[i]]$potential_issues$new))
    } else {
      "OK"
    }

    cat(sprintf("%s: %s", i, status), "\n")
    if (status != "OK") {
      if (!is.null(x[[i]]$issues)) print(x[[i]]$issues)
      if (!is.null(x[[i]]$potential_issues)) print(x[[i]]$potential_issues)
      cat("\n")
    }
  }
  invisible(x)
}

#' @export
print.issues <- function(x, ...) {
  cat(collapse_new_lines(x), sep = "\n\n")
  invisible(x)
}

#' @export
print.potential_issues <- function(x, ...) {
  for (i in seq_along(x$new)) {
    print(cli::diff_chr(
      strsplit(collapse_new_lines(x$old[i]), "\n")[[1]],
      strsplit(collapse_new_lines(x$new[i]), "\n")[[1]]
    ))
    cat("\n")
  }
  invisible(x)
}

strip_details_from_issue <- function(x) {
  x <- gsub(
    x = x,
    pattern = "See(.*?)for details",
    replacement = "See <path> for details"
  )
  gsub(
    x = x,
    pattern = "[[:space:]]",
    replacement = ""
  )
}

collapse_new_lines <- function(x) {
  gsub(
    x = x,
    pattern = "(\\n\\s*){2,}",
    replacement = "\n\n",
  )
}
