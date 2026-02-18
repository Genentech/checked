# CRAN does not like out subprocesses tests resulting in false positive tests 
# results
testthat::skip_on_cran()

test_that("check_pkgs works as expected", {
  examples_path <- system.file("example_packages", package = "checked")
<<<<<<< HEAD
=======
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
>>>>>>> 3615af88add43ec6f6d21ee450e4c8433c467911

  expect_no_error(
    plan <- check_pkgs(
      file.path(examples_path, c("exampleGood", "exampleBad")),
      n = 2L,
      repos = "https://cran.r-project.org/",
      reporter = NULL,
      lib.loc = .libPaths()
    )
  )
  
  r <- results(plan)
  expect_s3_class(r, "checked_results")
  expect_true(is.list(r))
  expect_length(
    list.dirs(file.path(plan$output, "checks"), recursive = FALSE),
    2
  )
  expect_named(r)
  expect_length(r, 1L)
  expect_length(r[[1]], 2L)

  expect_s3_class(r[[1]], "local_check_results")

  expect_length(r[[1]][[1]]$notes$issues, 1L)
  expect_length(r[[1]][[1]]$notes$potential_issues$new, 0L)
  expect_length(r[[1]][[1]]$notes$potential_issues$old, 0L)

  expect_length(r[[1]][[1]]$warnings$issues, 3L)
  expect_length(r[[1]][[1]]$warnings$potential_issues$new, 0L)
  expect_length(r[[1]][[1]]$warnings$potential_issues$old, 0L)

  expect_length(r[[1]][[1]]$errors$issues, 0L)
  expect_length(r[[1]][[1]]$errors$potential_issues$new, 0L)
  expect_length(r[[1]][[1]]$errors$potential_issues$old, 0L)

  expect_length(r[[1]][[2]]$notes$issues, 0L)
  expect_length(r[[1]][[2]]$notes$potential_issues$new, 0L)
  expect_length(r[[1]][[2]]$notes$potential_issues$old, 0L)

  expect_length(r[[1]][[2]]$warnings$issues, 0L)
  expect_length(r[[1]][[2]]$warnings$potential_issues$new, 0L)
  expect_length(r[[1]][[2]]$warnings$potential_issues$old, 0L)

  expect_length(r[[1]][[2]]$errors$issues, 0L)
  expect_length(r[[1]][[2]]$errors$potential_issues$new, 0L)
  expect_length(r[[1]][[2]]$errors$potential_issues$old, 0L)
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
