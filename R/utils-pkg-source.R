strip_src_contrib <- function(x, repos) {
  # Repositories are matched by prefix rather than equality. For CRAN-like
  # repositories the `Repository` field is exactly the contrib url, but some
  # repositories (notably R-universe) report it as the full, per-package
  # tarball url, e.g.
  # `https://<user>.r-universe.dev/src/contrib/<pkg>_<ver>.tar.gz?sha256=...&file=`
  match <- vlapply(repos, function(r) {
    startsWith(x, utils::contrib.url(r))
  })
  repos[match]
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

get_desc_field <- function(path, field) {
  desc <- file.path(path, "DESCRIPTION")
  if (!file.exists(desc)) {
    return(path)
  }
  read.dcf(desc)[, field]
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
    db <- available_packages(repos = repos)
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
