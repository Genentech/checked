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

pkg_dependencies <- function(
  packages,
  dependencies = TRUE,
  repos = getOption("repos")
) {
  dependencies <- as_pkg_dependencies(dependencies)
  unique(unlist(c(
    tools::package_dependencies(
      packages = packages,
      db = available_packages(repos = repos),
      recursive = FALSE,
      which = dependencies$direct
    ),
    tools::package_dependencies(
      packages = packages,
      db = available_packages(repos = repos),
      recursive = TRUE,
      which = dependencies$indirect
    )
  )))
}
