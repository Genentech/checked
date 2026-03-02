test_that("results_to_df works as expected", {
  # Entire testing suite takes too long on CRAN, we need to trim it
  skip_on_cran()

  examples_path <- system.file("example_packages", package = "checked")

  expect_no_error(
    plan <- check_pkgs(
      file.path(examples_path, c("exampleGood", "exampleBad")),
      n = 1L,
      repos = "https://cran.r-project.org/",
      reporter = NULL
    )
  )

  r <- results(plan)
  df <- results_to_df(r[[1]])
  expect_equal(NROW(df), 2)
  expect_equal(names(df), c("notes", "warnings", "errors"))
  expect_equal(df$notes, c(2, 0))
  expect_equal(df$warnings, c(3, 0))
  expect_equal(df$errors, c(0, 0))
  expect_true(
    all(
      endsWith(row.names(df)[[1]], "check-exampleBad"),
      endsWith(row.names(df)[[2]], "check-exampleGood")
    )
  )
})
