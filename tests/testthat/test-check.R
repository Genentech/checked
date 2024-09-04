test_that("check_pkgs works as expected", {
  examples_path <- system.file("example_packages", package = "checked")

  # WIP
  expect_no_error(check_pkgs(
    file.path(examples_path, c("exampleGood", "exampleBad")),
    n = 2L,
    repos = "https://cran.r-project.org/",
    reporter = NULL
  ))
})

test_that("check_pkgs works as expected", {
  examples_path <- system.file("example_packages", package = "checked")

  # WIP
  expect_no_error(check_pkgs(
    file.path(examples_path, c("exampleGood", "exampleBad")),
    n = 2L,
    repos = "https://cran.r-project.org/",
    reporter = NULL
  ))
})
