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
  expect_snapshot(df)
})
