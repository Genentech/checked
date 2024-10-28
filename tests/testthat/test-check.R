# CRAN does not like out subprocesses tests resulting in false positive tests 
# results
testthat::skip_on_cran()

test_that("check_pkgs works as expected", {
  examples_path <- system.file("example_packages", package = "checked")
  # WIP
  expect_no_error(design <- check_pkgs(c(
    file.path(examples_path, "exampleGood"),
    file.path(examples_path, "exampleBad")
  ), n = 2L, repos = "https://cran.r-project.org/",
  env = c(NOT_CRAN = "false", options::opt("check_envvars"))))
  
  expect_identical(
    design$input$package[[1]]$env, 
    c(NOT_CRAN = "false", options::opt("check_envvars"))
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

test_that("check design restore dialog test", {
  
  dir_create(output <- tempfile())
  expect_true(dir.exists(output))
  with_mocked_bindings({
    check_past_output(output, options::opt("restore"), ask = TRUE)
  }, restore_menu = function(...) "1") # Yes
  expect_true(dir.exists(output))
  with_mocked_bindings({
    check_past_output(output, options::opt("restore"), ask = TRUE)
  }, restore_menu = function(...) "2") # No
  expect_true(!dir.exists(output))
  
  dir_create(output <- tempfile())
  expect_true(dir.exists(output))
  check_past_output(output, options::opt("restore"), ask = FALSE)
  expect_true(!dir.exists(output))
  
  dir_create(output <- tempfile())
  expect_true(dir.exists(output))
  check_past_output(output, TRUE, ask = TRUE)
  expect_true(dir.exists(output))
  check_past_output(output, FALSE, ask = TRUE)
  expect_true(!dir.exists(output))
  
})
