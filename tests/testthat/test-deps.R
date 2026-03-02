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
