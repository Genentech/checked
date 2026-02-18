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
        n = 2L,
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
      n = 2L,
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
  expect_s3_class(r[[1]][[1]], "rcmdcheck_rev_dep_results")
  expect_s3_class(r[[1]][[2]], "rcmdcheck_rev_dep_results")

  # rev.both.error
  expect_length(r[[1]][[2]]$notes$issues, 0L)
  expect_length(r[[1]][[2]]$notes$potential_issues$new, 0L)
  expect_length(r[[1]][[2]]$notes$potential_issues$old, 0L)

  expect_length(r[[1]][[2]]$warnings$issues, 1L)
  expect_true(
    grepl("Namespace in Imports field not imported from",
          r[[1]][[2]]$warnings$issues),
    grepl("Missing or unexported object",
          r[[1]][[2]]$warnings$issues)
  )
  expect_length(r[[1]][[2]]$warnings$potential_issues$new, 0L)
  expect_length(r[[1]][[2]]$warnings$potential_issues$old, 0L)

  expect_length(r[[1]][[2]]$errors$issues, 1L)
  expect_true(
    grepl("Running the tests in",
          r[[1]][[2]]$errors$issues),
    grepl("is not an exported object from",
          r[[1]][[2]]$errors$issues)
  )
  expect_length(r[[1]][[2]]$errors$potential_issues$new, 0L)
  expect_length(r[[1]][[2]]$errors$potential_issues$old, 0L)
  
  # rev.both.ok
  expect_length(r[[1]][[1]]$notes$issues, 0L)
  expect_length(r[[1]][[1]]$notes$potential_issues$new, 0L)
  expect_length(r[[1]][[1]]$notes$potential_issues$old, 0L)

  expect_length(r[[1]][[1]]$warnings$issues, 0L)
  expect_length(r[[1]][[1]]$warnings$potential_issues$new, 0L)
  expect_length(r[[1]][[1]]$warnings$potential_issues$old, 0L)

  expect_length(r[[1]][[1]]$errors$issues, 0L)
  expect_length(r[[1]][[1]]$errors$potential_issues$new, 0L)
  expect_length(r[[1]][[1]]$errors$potential_issues$old, 0L)
})

test_that("check_rev_deps works for a package without a version in repos", {
  # Ensure source installation to make sure test works also on mac and windows
  withr::with_options(list(pkgType = "source"), {
    expect_no_error(design <- check_rev_deps(
      file.path(sources_new, "pkg.suggests"),
      n = 2L,
      repos = repo,
      reporter = NULL
    ))
  })

  r <- results(design)
  expect_s3_class(r, "checked_results")
  expect_true(is.list(r))
  expect_length(
    list.dirs(file.path(design$output, "checks"), recursive = FALSE),
    2
  )
  expect_named(r)
  expect_length(r, 1L)
  expect_length(r[[1]], 1L)

  expect_s3_class(r[[1]], "rev_dep_dep_results")
  expect_s3_class(r[[1]][[1]], "rcmdcheck_rev_dep_results")

  expect_length(r[[1]][[1]]$notes$issues, 0L)
  expect_length(r[[1]][[1]]$notes$potential_issues$new, 0L)
  expect_length(r[[1]][[1]]$notes$potential_issues$old, 0L)

  expect_length(r[[1]][[1]]$warnings$issues, 0L)
  expect_length(r[[1]][[1]]$warnings$potential_issues$new, 0L)
  expect_length(r[[1]][[1]]$warnings$potential_issues$old, 0L)

  expect_length(r[[1]][[1]]$errors$issues, 1L)
  expect_true(
    grepl("Running the tests in", r[[1]][[1]]$errors$issues)
  )
  expect_true(
    grepl("Reverse suggested deps detected", r[[1]][[1]]$errors$issues)
  )
  expect_length(r[[1]][[1]]$errors$potential_issues$new, 0L)
  expect_length(r[[1]][[1]]$errors$potential_issues$old, 0L)
})
