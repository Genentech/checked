test_that("check_packages works as expected", {
  examples_path <- system.file("example_packages", package = "checked")
  # WIP
  expect_no_error(check_packages(c(
    file.path(examples_path, "exampleGood"),
    file.path(examples_path, "exampleBad")
  ), n = 2L, repos = "https://cran.r-project.org/"))
})
