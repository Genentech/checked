test_that("strip_src_contrib identifies the originating repository", {
  repos <- c(
    "https://ddsjoberg.r-universe.dev",
    "https://cloud.r-project.org"
  )

  # CRAN-like repositories report `Repository` as the contrib url itself
  expect_identical(
    strip_src_contrib(
      utils::contrib.url("https://cloud.r-project.org"),
      repos = repos
    ),
    "https://cloud.r-project.org"
  )

  # R-universe reports `Repository` as the full, per-package tarball url
  runiverse_repository <- paste0(
    utils::contrib.url("https://ddsjoberg.r-universe.dev"),
    "/gtsummary_2.5.1.9015.tar.gz?sha256=962c&file="
  )

  expect_identical(
    strip_src_contrib(runiverse_repository, repos = repos),
    "https://ddsjoberg.r-universe.dev"
  )
})

test_that("strip_src_contrib returns nothing for unknown repositories", {
  expect_identical(
    strip_src_contrib(
      utils::contrib.url("https://example.com/other"),
      repos = "https://cloud.r-project.org"
    ),
    character(0L)
  )
})

test_that("get_package_source builds source archive urls", {
  db <- matrix(
    c("gtsummary", "2.5.1.9015", NA_character_),
    nrow = 1L,
    dimnames = list(
      "gtsummary",
      c("Package", "Version", "Repository")
    )
  )

  db[, "Repository"] <- utils::contrib.url("https://cloud.r-project.org")
  expect_identical(
    get_package_source("gtsummary", repos = NULL, db = db),
    paste0(
      utils::contrib.url("https://cloud.r-project.org"),
      "/gtsummary_2.5.1.9015.tar.gz"
    )
  )

  # R-universe `Repository` fields are crafted so that appending the package
  # file name (as `utils::download.packages()` does) yields a valid url
  db[, "Repository"] <- paste0(
    utils::contrib.url("https://ddsjoberg.r-universe.dev"),
    "/gtsummary_2.5.1.9015.tar.gz?sha256=962c&file="
  )
  expect_identical(
    get_package_source("gtsummary", repos = NULL, db = db),
    paste0(
      utils::contrib.url("https://ddsjoberg.r-universe.dev"),
      "/gtsummary_2.5.1.9015.tar.gz?sha256=962c",
      "&file=/gtsummary_2.5.1.9015.tar.gz"
    )
  )
})

test_that("pkg_origin_repo resolves packages from an R-universe repository", {
  skip_on_cran()

  repos <- c(
    "https://ddsjoberg.r-universe.dev",
    "https://cloud.r-project.org"
  )

  db <- tryCatch(
    available_packages(repos = repos),
    warning = function(w) skip("repositories are not reachable")
  )
  skip_if_not("gtsummary" %in% rownames(db))

  origin <- pkg_origin_repo("gtsummary", repos = repos)
  expect_identical(
    unname(origin$repos),
    "https://ddsjoberg.r-universe.dev"
  )
  expect_match(
    check_path(origin, output = NULL),
    "^https://ddsjoberg\\.r-universe\\.dev/.+\\.tar\\.gz"
  )
})
