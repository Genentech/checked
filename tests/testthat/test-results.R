test_that("results_to_file works as expected", {
  examples_path <- system.file("example_packages", package = "checked")
  # WIP
  expect_no_error(plan <- check_pkgs(c(
    file.path(examples_path, "exampleGood"),
    file.path(examples_path, "exampleBad")
  ), n = 2L, repos = "https://cran.r-project.org/"))

  r <- results(plan)
  r_file <- tempfile()
  expect_no_error(results_to_file(r, r_file))
  expect_true(!identical(readLines(r_file), "No issues identified."))

  expect_no_error(plan <- check_rev_deps(c(
    file.path(examples_path, "exampleBad")
  ), n = 2L, repos = "https://cran.r-project.org/"))

  r <- results(plan)
  r_file <- tempfile()
  expect_no_error(results_to_file(r, r_file))
  expect_true(identical(readLines(r_file), "No issues identified."))
})
