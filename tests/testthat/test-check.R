test_that("check_pkgs works as expected", {
  examples_path <- system.file("example_packages", package = "checked")
  # WIP
  expect_no_error(design <- check_pkgs(c(
    file.path(examples_path, "exampleGood"),
    file.path(examples_path, "exampleBad")
  ), n = 2L, repos = "https://cran.r-project.org/",
  env = c(NOT_CRAN = "false")))
  
  expect_identical(
    design$input$package[[1]]$env, 
    c("NOT_CRAN" = "false", DEFAULT_CHECK_ENV_VARIABLES)
  )
})

test_that("check_pkgs works as expected", {
  examples_path <- system.file("example_packages", package = "checked")
  # WIP
  expect_no_error(check_pkgs(
    c(
      file.path(examples_path, "exampleGood"),
      file.path(examples_path, "exampleBad")
    ),
    n = 2L, repos = "https://cran.r-project.org/",
    reporter = checked:::reporter_ansi_tty()
  ))
})
