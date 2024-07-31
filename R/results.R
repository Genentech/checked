CHECK_ISSUES_TYPES <- c("notes", "warnings", "errors")

#' Check results
#' 
#' Get R CMD check results
#' 
#' @param x \code{\link[checked]{check_design}} object.
#' @param error_on character vector indicating whether R error should be thrown
#' when issues are discovered when generating results. "never" means that no
#' errors are thrown. If "issues" then errors are emitted only on issues, whereas
#' "potential issues" stands for error on both issues and potential issues. Users 
#' can set the default value via env variable \code{CHECKED_RESULTS_ERROR_ON}.
#' @param ... other parameters.
#' 
#' @export
results <- function(x, ...) {
  UseMethod("results")
}

#' @export
#' @rdname results
results.check_design <- function(
    x, 
    error_on = Sys.getenv("CHECKED_RESULTS_ERROR_ON", c("never", "issues", "potential_issues")[1]), 
    ...) {
  
  error_on <- match.arg(error_on, c("never", "issues", "potential_issues"))
  checks_nodes <- igraph::V(x$graph)[igraph::vertex.attributes(x$graph)$type == "check"]
  checks_classes <- vcapply(checks_nodes$spec, function(x) class(x)[[1]])
  classes <- unique(checks_classes)
  res <- lapply(classes, function(x) {
    structure(
      checks_nodes$spec[checks_classes == x],
      class = paste0("list_", x)
    )
  })
  
  res <- structure(
    lapply(res, function(y, output) {
      structure(
        results(y, output),
        class = paste0("checked_results_", utils::head(class(y[[1]]), 1L))
      )
    }, output = x$output),
    names = classes,
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
`[.checked_results` <- function(x, ...) {
  structure(NextMethod(), class = class(x))
}

#' @export
#' @noRd
results.list_revdep_check_task_spec <- function(x, output, ...) {
  name <- vcapply(x, function(y) y$package_spec$name)
  revdep <- vcapply(x, `[[`, "revdep")
  
  new <- lapply(sort(unique(name)), function(y) {
    x[[which(name == y & revdep == "new")]]
  })
  
  old <- lapply(sort(unique(name)), function(y) {
    x[[which(name == y & revdep == "old")]]
  })
  
  structure(
    mapply(results, x = new, y = old, output = output, SIMPLIFY = FALSE),
    names = sort(unique(name))
  )
}

#' @export
#' @noRd
results.revdep_check_task_spec <- function(x, y, output, ...) {
  new <- rcmdcheck_from_json(file.path(path_check_output(output, x$alias), "result.json"))
  old <- rcmdcheck_from_json(file.path(path_check_output(output, y$alias), "result.json"))

  structure(
    lapply(CHECK_ISSUES_TYPES, function(i) {
      new_i <- structure(
        new[[i]],
        names = get_issue_header(new[[i]])
      )
      old_i <- structure(
        old[[i]],
        names = get_issue_header(old[[i]])
      )
      
      matching_headers_idx <- names(new_i) %in% names(old_i)
      # Create temporary object with "See <path> for details" path
      # stripped out as it will always emit potential issues due to the path 
      # differences
      new_i_tmp <- strip_details_from_issue(new_i)
      old_i_tmp <- strip_details_from_issue(old_i)
      matching_messages_idx <- new_i_tmp %in% old_i_tmp
      
      new_issues <- structure(
        unname(new_i[!matching_headers_idx]),
        class = "issues"
      )
      new_potential_issues <- new_i[matching_headers_idx & !matching_messages_idx]
      new_potential_issues <- structure(
        list(
          new = unname(new_potential_issues),
          old = unname(old_i[names(new_potential_issues)])
        ),
        class = "potential_issues"
      )
      
      list("issues" = new_issues, "potential_issues" = new_potential_issues)
    }),
    names = CHECK_ISSUES_TYPES,
    package = new$package,
    class = "rcmdcheck_diff"
  )
}

#' @export
#' @noRd
results.list_check_task_spec <- function(x, output, ...) {
  alias <- vcapply(x, `[[`, "alias")
  structure(
    lapply(x, results, output = output),
    names = alias
  )
}

#' @export
#' @noRd
results.check_task_spec <- function(x, output, ...) {
  x <- rcmdcheck_from_json(file.path(path_check_output(output, x$alias), "result.json"))
  
  structure(
    lapply(CHECK_ISSUES_TYPES, function(i) {
      x_i <- x[[i]]
      
      new_issues <- structure(
        unname(x_i),
        class = "issues"
      )
      
      list("issues" = new_issues)
    }),
    names = CHECK_ISSUES_TYPES,
    package = x$package,
    class = "rcmdcheck_diff"
  )
}

#' Results to file
#' 
#' Write \code{checked_results} object to the text file. When converting results
#' to text, \code{\link[checked]{print.checked_results}} method is used.
#' 
#' 
#' @param results \code{\link[checked]{results}} object.
#' @param file A connection or character path.
#' @inheritParams print.checked_results
#' 
#' @export
results_to_file <- function(results, file, keep = "all", ...) {
  text <- c()
  for (i in seq_along(results)) {
    df <- results_to_df(results[[i]], issues_type = keep)
    if (keep == "all" || any(rowSums(df) > 0)) {
      text <- c(
        text,
        utils::capture.output(print(results[i], keep = keep))
      )
    }
  }
  
  if (!any(nzchar(text))) {
    text <- "No issues identified."
  }
  
  writeLines(text, file) 
}

results_to_df <- function(results, ...) {
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

#' @export
summary.check_design <- function(object, ...) {
  summary(results(object), ...)
}

#' @export
summary.checked_results <- function(object, ...) {
  lapply(object, summary, ...)
}

#' @export
summary.checked_results_revdep_check_task_spec <- function(object, ...) {
  summary.checked_results_check_task_spec(object, ...)
}

#' @export
summary.checked_results_check_task_spec <- function(object, ...) {
  results_to_df(object, ...)
}

#' Print checked results
#'
#' @param x an object to be printed.
#' @param keep character vector indicating which packages should be included
#' in the results. "all" means that all packages are kept. If "issues" then 
#' only packages with issues identified, whereas "potential_issues" stands for
#' keeping packages with both "issues" and "potential_issues". Users can set
#' the default value via env variable \code{CHECKED_RESULTS_KEEP}.
#' @param ... other parameters.
#' 
#' @rdname print.checked_results
#' @export
print.checked_results <- function(x, ...) {
  for (i in seq_along(x)) {
    cat("#", tools::toTitleCase(strsplit(names(x)[i], "_")[[1]]), "\n\n")
    print(x[[i]], ...)
    cat("\n")
  }
  invisible(x)
}

#' @rdname print.checked_results
#' @export
print.checked_results_check_task_spec <- function(
    x, 
    keep = Sys.getenv("CHECKED_RESULTS_KEEP", c("all", "issues", "potential_issues")[1]),
    ...) {
  
  keep <- match.arg(keep, c("all", "issues", "potential_issues"))
  if (keep != "all") {
    df <- results_to_df(x, issues_type = keep)
    issues <- rowSums(df) != 0
    x <- x[issues]
  }
  
  for (i in seq_along(x)) {
    print(x[[i]], ...)
    cat("\n")
  }
  invisible(x)
}

#' @rdname print.checked_results
#' @export
print.checked_results_revdep_check_task_spec <- function(x, ...) {
  print.checked_results_check_task_spec(x, ...)
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
print.rcmdcheck_diff <- function(x, ...) {
  cat(sprintf("%s package R CMD check diff \n", attr(x, "package")))
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
  cat(x, sep = "\n\n")
  invisible(x)
}

#' @export
print.potential_issues <- function(x, ...) {
  for (i in seq_along(x$new)) {
    print(cli::diff_chr(
      strsplit(x$old[i], "\n")[[1]],
      strsplit(x$new[i], "\n")[[1]]
    ))
    cat("\n")
  }
  invisible(x)
}

strip_details_from_issue <- function(x) {
  gsub("See(.*?)for details", "See <path> for details", x)
}
