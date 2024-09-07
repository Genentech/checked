strip_src_contrib <- function(x) {
  sub("/src/contrib$", "", x)
}

split_packages_names <- function(x) {
  if (is.na(x)) {
    return(x)
  }
  vcapply(.tools$.split_dependencies(x), "[[", "name", USE.NAMES = FALSE)
}

check_dependencies <- function(dependencies) {
  is_all <- isTRUE(dependencies)
  is_strong <- length(dependencies) == 1 && is.na(dependencies)

  dependencies <- if (dependencies == "all" || is_all) {
    as.character(unlist(DEP))
  } else if (dependencies == "strong" || is_strong) {
    as.character(unlist(DEP_STRONG))
  } else if (is.character(dependencies)) {
    valid_deps <- dependencies %in% unlist(DEP)
    if (!all(valid_deps)) {
      warning(
        "Passed dependencies names does not match standard R dependencies ",
        "names. The non-standard names has been removed."
      )
    }
    dependencies[valid_deps]
  } else {
    stop(
      "Dependencies has to be a TRUE/NA logical or character vector with ",
      "names of standard R dependencies."
    )
  }

  stopifnot(is.character(dependencies), length(dependencies) >= 1)
  dependencies
}

bind_descs <- function(x, fields = NULL) {
  all_colnames <- unique(unlist(lapply(x, colnames)))
  if (!is.null(fields)) {
    all_colnames <- intersect(all_colnames, fields)
  }
  for (i in seq_along(x)) {
    missing_cols <- setdiff(all_colnames, colnames(x[[i]]))
    new_cols <- rep_len(NA, length(missing_cols))
    names(new_cols) <- missing_cols
    x[[i]] <- do.call(cbind, c(list(x[[i]]), new_cols))
    x[[i]] <- x[[i]][, all_colnames, drop = FALSE]
  }
  do.call(rbind, x)
}

get_desc <- function(origin) {
  UseMethod("get_desc")
}

get_desc_field <- function(path, field) {
  desc <- file.path(path, "DESCRIPTION")
  if (!file.exists(desc)) {
    return(path)
  }
  read.dcf(desc)[, field]
}

sub_aliased_desc <- function(desc) {
  if (!"Alias" %in% colnames(desc)) return(desc)

  # create mapping from package name to aliased name
  has_alias <- !is.na(desc[, "Alias"])
  alias_map <- desc[has_alias, "Package"]
  names(alias_map) <- desc[has_alias, "Alias"]

  for (deptype in names(DEP)) {
    for (i in seq_len(nrow(desc))) {
      desc[[i, deptype]] <- sub_aliased_deps(desc[[i, deptype]], alias_map)
    }
  }

  desc
}

#' Substitute Aliased Dependencies String
#'
#' @param deps `character(1L)`, as provided by a `DESCRIPTION` file's
#'   dependency listing.
#' @param aliases (named vector), a mapping of package names to aliases for
#'   substitution.
#' @return `deps`, substituting package names with associated aliases.
#'
sub_aliased_deps <- function(deps, aliases) {
  if (is.na(deps)) {
    return(deps)
  }

  deps <- .tools$.split_dependencies(deps)
  for (i in seq_along(deps)) {
    if (deps[[i]]$name %in% names(aliases)) {
      deps[[i]]$name <- aliases[[deps[[i]]$name]]
    }
  }

  paste(collapse = ", ", vcapply(deps, function(dep) {
    paste0(
      dep$name,
      if (!is.null(dep$version)) {
        paste0(" (", dep$op, " ", dep$version, ")")
      }
    )
  }))
}

get_package_name <- function(path) {
  get_desc_field(path, "Package")
}

get_package_version <- function(path) {
  get_desc_field(path, "Version")
}

fetch_package_source <- function(archive_url, destdir) {
  bn <- basename(archive_url)
  destfile <- file.path(destdir, bn)
  if (!file.exists(destfile)) {
    utils::download.file(archive_url, destfile = destfile, quiet = TRUE)
  }
  destfile
}

get_package_source <- function(package, repos, db = NULL, destdir = NULL) {
  if (is.null(db)) {
    db <- utils::available.packages(repos = repos)
  }
  pkg <- db[package, ]
  archive_url <- sprintf(
    "%s/%s_%s.tar.gz",
    pkg["Repository"],
    pkg["Package"],
    pkg["Version"]
  )

  if (!is.null(destdir)) {
    fetch_package_source(archive_url, destdir)
  } else {
    archive_url
  }
}

package_deps <- function(packages = NULL, ...) {
  if (length(packages) == 0) {
    NULL
  } else {
    tools::package_dependencies(packages = packages, ...)
  }
}
