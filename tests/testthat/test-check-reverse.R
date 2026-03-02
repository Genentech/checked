create_temp_repo <- function(dir, repo_path) {
  contrib_url <- utils::contrib.url(repo_path, type = "source")
  dir.create(contrib_url, recursive = TRUE)
  sources <- list.files(dir, pattern = "^.*\\.tar\\.gz$", full.names = TRUE)
  vapply(sources, file.copy, FUN.VALUE = logical(1), to = contrib_url)
  tools::write_PACKAGES(contrib_url, type = "source")
}

sources_old <- test_path("fixtures", "revdeps", "v1")
sources_new <- test_path("fixtures", "revdeps", "v2")

dir.create(repo_dir <- tempfile("repo"))
repo <- paste0("file:///", repo_dir)
create_temp_repo(sources_old, repo_dir)

test_that("check_rev_deps works for package with no revdeps", {
  # Ensure source installation to make sure test works also on mac and windows
  withr::with_options(list(pkgType = "source"), {
    expect_no_error(
      checks <- check_rev_deps(
        file.path(sources_new, "pkg.none"),
        n = 1L,
        repos = repo,
        reporter = NULL
      )
    )
  })

  r <- results(checks)
  expect_s3_class(r, "checked_results")
  expect_true(is.list(r))
  expect_length(r, 0L)
})


test_that("check_rev_deps works for package with one breaking change", {
  withr::with_options(list(pkgType = "source"), {
    design <- check_rev_deps(
      file.path(sources_new, "pkg.ok.error"),
      n = 1L,
      repos = repo,
      reporter = NULL
    )
  })

  r <- results(design)
  expect_s3_class(r, "checked_results")
  expect_true(is.list(r))
  expect_named(r)
  expect_length(r, 1L)
  expect_length(r[[1]], 2L)
  expect_s3_class(r[[1]], "rev_dep_dep_results")

  # rev.both.error
  both_error_i <- which(grepl("rev.both.error", names(r[[1]]), fixed = TRUE))
  expect_true(length(both_error_i) == 1)
  r_both_error <- r[[1]][[both_error_i]]
  expect_s3_class(r_both_error, "rcmdcheck_rev_dep_results")

  expect_length(r_both_error$notes$issues, 0L)
  expect_length(r_both_error$notes$potential_issues$new, 0L)
  expect_length(r_both_error$notes$potential_issues$old, 0L)

  expect_length(r_both_error$warnings$issues, 1L)
  expect_true(
    grepl("Missing or unexported object", r_both_error$warnings$issues)
  )
  expect_length(r_both_error$warnings$potential_issues$new, 0L)
  expect_length(r_both_error$warnings$potential_issues$old, 0L)

  expect_length(r_both_error$errors$issues, 1L)
  expect_true(
    grepl("Running the tests in", r_both_error$errors$issues)
  )
  expect_true(
    grepl("is not an exported object from", r_both_error$errors$issues)
  )
  expect_length(r_both_error$errors$potential_issues$new, 0L)
  expect_length(r_both_error$errors$potential_issues$old, 0L)

  # rev.both.ok
  both_ok_i <- which(grepl("rev.both.ok", names(r[[1]]), fixed = TRUE))
  expect_true(length(both_ok_i) == 1)
  r_both_ok <- r[[1]][[both_ok_i]]
  expect_s3_class(r_both_ok, "rcmdcheck_rev_dep_results")

  expect_length(r_both_ok$notes$issues, 0L)
  expect_length(r_both_ok$notes$potential_issues$new, 0L)
  expect_length(r_both_ok$notes$potential_issues$old, 0L)

  expect_length(r_both_ok$warnings$issues, 0L)
  expect_length(r_both_ok$warnings$potential_issues$new, 0L)
  expect_length(r_both_ok$warnings$potential_issues$old, 0L)

  expect_length(r_both_ok$errors$issues, 0L)
  expect_length(r_both_ok$errors$potential_issues$new, 0L)
  expect_length(r_both_ok$errors$potential_issues$old, 0L)
})

test_that("check_rev_deps works for a package without a version in repos", {
  # Ensure source installation to make sure test works also on mac and windows
  withr::with_options(list(pkgType = "source"), {
    expect_no_error(design <- check_rev_deps(
      file.path(sources_new, "pkg.suggests"),
      n = 1L,
      repos = repo,
      reporter = NULL
    ))
  })

  r <- results(design)
  expect_s3_class(r, "checked_results")
  expect_true(is.list(r))
  expect_length(
    list.dirs(file.path(design$output, "checks"), recursive = FALSE),
    4
  )
  expect_named(r)
  expect_length(r, 1L)
  expect_length(r[[1]], 2L)
  expect_s3_class(r[[1]], "rev_dep_dep_results")

  # pkg.none.broken
  none_broken_i <- which(grepl("pkg.none.broken-", names(r[[1]]), fixed = TRUE))
  expect_true(length(none_broken_i) == 1)
  r_none_broken <- r[[1]][[none_broken_i]]
  expect_s3_class(r_none_broken, "rcmdcheck_rev_dep_results")

  expect_length(r_none_broken$notes$issues, 0L)
  expect_length(r_none_broken$notes$potential_issues$new, 0L)
  expect_length(r_none_broken$notes$potential_issues$old, 0L)

  expect_length(r_none_broken$warnings$issues, 0L)
  expect_length(r_none_broken$warnings$potential_issues$new, 0L)
  expect_length(r_none_broken$warnings$potential_issues$old, 0L)

  expect_length(r_none_broken$errors$issues, 1L)
  expect_true(
    grepl("Running the tests in", r_none_broken$errors$issues)
  )
  expect_true(
    grepl("there is no package called", r_none_broken$errors$issues)
  )
  expect_length(r_none_broken$errors$potential_issues$new, 0L)
  expect_length(r_none_broken$errors$potential_issues$old, 0L)


  # pkg.none
  none_i <- which(grepl("pkg.none-", names(r[[1]])))
  expect_true(length(none_i) == 1)
  r_none <- r[[1]][[none_i]]
  expect_s3_class(r_none, "rcmdcheck_rev_dep_results")

  expect_length(r_none$notes$issues, 0L)
  expect_length(r_none$notes$potential_issues$new, 0L)
  expect_length(r_none$notes$potential_issues$old, 0L)

  expect_length(r_none$warnings$issues, 0L)
  expect_length(r_none$warnings$potential_issues$new, 0L)
  expect_length(r_none$warnings$potential_issues$old, 0L)

  expect_length(r_none$errors$issues, 1L)
  expect_true(
    grepl("Running the tests in", r_none$errors$issues)
  )
  expect_true(
    grepl("Reverse suggested deps detected", r_none$errors$issues)
  )
  expect_length(r_none$errors$potential_issues$new, 0L)
  expect_length(r_none$errors$potential_issues$old, 0L)

  # Plot works
  expect_no_error(plot(design$graph))
  expect_no_error(
    g_i <- plot(design$graph, interactive = TRUE)
  )
  expect_s3_class(g_i, "visNetwork")
})
