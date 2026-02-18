test_that("check_pkgs works as expected", {
  examples_path <- system.file("example_packages", package = "checked")

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
