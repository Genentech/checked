#' Convert a value to a set of dependency types
#'
#' Implements conventions established by both [`tools::package_dependencies()`]
#' and [`pkgdepends::as_pkg_dependencies()`], following
#' [`pkgdepends::as_pkg_dependencies()`] return type structure of a list
#' including `$direct` and `$indirect` dependency types. Reimplemented to
#' avoid dependence on [`pkgdepends`] compilation requirements.
#'
#' @param x A `logical` scalar, `character` string of `"all"`, `"most"`,
#'   `"hard"` or `"soft"`, `NA` or a vector of dependency types.
#'
#' @note locally defined and bespoke dispatch system to avoid registering
#'   methods when loaded in combination with `pkgdepends`
#'
#' @keywords internal
as_pkg_dependencies <- function(x) {
  into_deptype_vec <- function(x) {
    if (is.na(x)) {
      x <- "hard"
    } else if (identical(x, TRUE)) {
      x <- "most"
    } else if (identical(x, FALSE)) {
      x <- character(0L)
    }

    deptypes <- c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")
    if (is.character(x)) {
      x <- switch(x,
        "all" = deptypes,
        "most" = c("Depends", "Imports", "LinkingTo", "Suggests"),
        "hard" = c("Depends", "Imports", "LinkingTo"),
        "soft" = c("Suggests", "Enhances"),
        x
      )
    }

    stopifnot(all(x %in% deptypes))

    x
  }

  into_deptype_list <- function(direct = "all", indirect = "hard") {
    list(
      direct = into_deptype_vec(direct),
      indirect = into_deptype_vec(indirect)
    )
  }

  if (!is.list(x)) x <- list(x)
  do.call(into_deptype_list, x)
}

#' Build Package Dependencies Table
#'
#' Inspired by `tools::package_dependencies`, but with the added benefit
#' of recording the dependency type and relationships throughout the
#' dependency tree.
#'
pkg_dependencies <- function(
  packages,
  dependencies = TRUE,
  db = available.packages(),
  verbose = FALSE
) {
  dependencies <- as_pkg_dependencies(dependencies)
  na_version <- package_version(NA_character_, strict = FALSE)
  proto_df <- data.frame(
    package = character(0L),
    type = character(0L),
    name = character(0L),
    op = character(0L),
    version = na_version[c()]
  )

  depth <- 0L
  out <- list()
  while (length(packages) > 0L) {
    depth <- depth + 1L
    deptypes <- if (depth == 1L) dependencies$direct else dependencies$indirect
    depstrs <- db[packages, deptypes, drop = FALSE]

    n <- length(out) + 1
    out[[n]] <- Map(
      package = rep(packages, times = length(deptypes)),
      depstr = as.vector(depstrs),
      deptype = rep(deptypes, each = length(packages)),
      f = function(package, depstr, deptype) {
        if (is.na(depstr)) return()
        deps <- tools:::.split_dependencies(depstr)
        out <- proto_df[seq_along(deps), , drop = FALSE]
        rownames(out) <- NULL

        out$package <- package
        out$type <- deptype
        out$name <- vcapply(deps, `[[`, "name")
        out$op <- vcapply(deps, function(i) i$op %||% NA_character_)
        out$version <- lapply(deps, function(i) i$version %||% na_version)

        out
      }
    )

    old_deps <- unlist(lapply(out, names))
    new_deps <- unlist(lapply(out[[n]], `[[`, "name"))
    packages <- setdiff(new_deps, c(old_deps, base_pkgs()))
  }

  out <- do.call(rbind, unlist(out, recursive = FALSE))
  rownames(out) <- NULL
  out
}
