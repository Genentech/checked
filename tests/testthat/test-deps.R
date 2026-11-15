test_that("pkg_dependencies works as expected for local package", {
  desc <- read.dcf(system.file("DESCRIPTION", package = "checked"))
  rownames(desc) <- "checked"

  local_deps <- pkg_dependencies(
    packages = "checked",
    dependencies = TRUE,
    db = desc
  )
  expect_snapshot(local_deps)
})


test_that("pkg_dependencies works as expected for cran package", {
  skip_on_cran()
  df <- pkg_dependencies(
    "checked",
    db = available_packages(
      repos = "https://packagemanager.posit.co/cran/2026-02-01"
    )
  )

  ap <- available_packages(
    repos = "https://packagemanager.posit.co/cran/2026-02-01"
  )
  core_pkgs <- ap[!is.na(ap[, "Priority"]), "Package"]
  df[df$package %in% core_pkgs & df$name == "R", ]$version <-
    list(package_version(NA_character_, strict = FALSE))
  # We need to exclude dependencies lines stating minimal R
  # version required for base and recommended packages as these will fail
  # for different version of R regardless of used snapshot
  expect_snapshot(df)
})

test_that("pkg_dependencies handles duplicated dependency entries", {
  # CRAN allows the same package to be listed more than once within a single
  # dependency field, e.g. crane lists `scales, scales (>= 1.3.0)` in Imports
  desc <- matrix(
    c(
      "dup",
      "R (>= 4.2)",
      "cli, scales, glue (>= 1.6.0), scales (>= 1.3.0), utils (>= 4.0.0), utils (>= 4.5.0)", # nolint
      "withr, withr"
    ),
    nrow = 1L,
    dimnames = list("dup", c("Package", "Depends", "Imports", "Suggests"))
  )

  deps <- expect_no_error(pkg_dependencies(
    packages = "dup",
    dependencies = TRUE,
    db = desc
  ))

  imports <- deps[deps$type == "Imports", ]
  expect_identical(imports$name, c("cli", "glue", "scales", "utils"))
  expect_identical(
    imports$version[imports$name == "scales"][[1L]],
    package_version("1.3.0")
  )
  expect_identical(
    imports$version[imports$name == "utils"][[1L]],
    package_version("4.5.0")
  )

  suggests <- deps[deps$type == "Suggests", ]
  expect_identical(suggests$name, "withr")
  expect_true(is.na(suggests$version[[1L]]))
})
