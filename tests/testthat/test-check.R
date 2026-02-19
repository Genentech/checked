test_that("check_pkgs works as expected", {
  examples_path <- system.file("example_packages", package = "checked")

  expect_no_error(
    plan <- check_pkgs(
      file.path(examples_path, c("exampleGood", "exampleBad")),
      n = 1L,
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

  # exampleBad
  example_bad_i <- which(grepl("check-exampleBad", names(r[[1]]), fixed = TRUE))
  expect_true(length(example_bad_i) == 1)
  r_example_bad <- r[[1]][[example_bad_i]]
  expect_s3_class(r_example_bad, "rcmdcheck_check_results")

  expect_length(r_example_bad$notes$issues, 2L)
  expect_length(r_example_bad$notes$potential_issues$new, 0L)
  expect_length(r_example_bad$notes$potential_issues$old, 0L)

  expect_length(r_example_bad$warnings$issues, 3L)
  expect_length(r_example_bad$warnings$potential_issues$new, 0L)
  expect_length(r_example_bad$warnings$potential_issues$old, 0L)

  expect_length(r_example_bad$errors$issues, 0L)
  expect_length(r_example_bad$errors$potential_issues$new, 0L)
  expect_length(r_example_bad$errors$potential_issues$old, 0L)

  # exampleGood
  example_good_i <-
    which(grepl("check-exampleGood", names(r[[1]]), fixed = TRUE))
  expect_true(length(example_good_i) == 1)
  r_example_good <- r[[1]][[example_good_i]]
  expect_s3_class(r_example_good, "rcmdcheck_check_results")

  expect_length(r_example_good$notes$issues, 0L)
  expect_length(r_example_good$notes$potential_issues$new, 0L)
  expect_length(r_example_good$notes$potential_issues$old, 0L)

  expect_length(r_example_good$warnings$issues, 0L)
  expect_length(r_example_good$warnings$potential_issues$new, 0L)
  expect_length(r_example_good$warnings$potential_issues$old, 0L)

  expect_length(r_example_good$errors$issues, 0L)
  expect_length(r_example_good$errors$potential_issues$new, 0L)
  expect_length(r_example_good$errors$potential_issues$old, 0L)
})
