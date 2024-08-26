create_temp_repo <- function(dir, repo_path) {
  dir.create(ctburl <- utils::contrib.url(repo_path, type = "source"), recursive = TRUE)
  sources <- list.files(dir, pattern = "^.*\\.tar\\.gz$", full.names = TRUE)
  vapply(sources, file.copy, FUN.VALUE = logical(1), to = ctburl)
  tools::write_PACKAGES(ctburl, type = "source")
}

sources_old <- test_path("testing_pkgs", "revdeps", "v1")
sources_new <- test_path("testing_pkgs", "revdeps", "v2")

dir.create(repo_dir <- tempfile("repo"))
repo <- paste0("file://", repo_dir)
create_temp_repo(sources_old, repo_dir)

test_that("check_rev_deps works for package with no revdeps", {
  # Ensure source installation to make sure test works also on mac and windows
  withr::with_options(list(pkgType = "source"), {
    expect_no_error(design <- check_rev_deps(
      file.path(sources_new, "pkg.none"),
      n = 2L, repos = repo))
  })

  r <- results(design)
  expect_s3_class(r, "checked_results")
  expect_true(is.list(r))
  expect_length(r, 0L)
})


test_that("check_rev_deps works for package with one breaking change", {
  withr::with_options(list(pkgType = "source"), {
    design <- check_rev_deps(
      file.path(sources_new, "pkg.ok.error"),
      n = 2L, repos = repo)
  })

  r <- results(design)
  expect_s3_class(r, "checked_results")
  expect_true(is.list(r))
  expect_named(r)
  expect_length(r, 1L)
  expect_length(r$revdep_check_task_spec, 2L)

  # rev.both.error
  expect_length(r$revdep_check_task_spec$rev.both.error$notes$issues, 0L)
  expect_length(r$revdep_check_task_spec$rev.both.error$notes$potential_issues$new, 0L)
  expect_length(r$revdep_check_task_spec$rev.both.error$notes$potential_issues$old, 0L)

  expect_length(r$revdep_check_task_spec$rev.both.error$warnings$issues, 1L)
  expect_true(
    grepl("Namespace in Imports field not imported from",
          r$revdep_check_task_spec$rev.both.error$warnings$issues),
    grepl("Missing or unexported object",
          r$revdep_check_task_spec$rev.both.error$warnings$issues)
  )
  expect_length(r$revdep_check_task_spec$rev.both.error$warnings$potential_issues$new, 0L)
  expect_length(r$revdep_check_task_spec$rev.both.error$warnings$potential_issues$old, 0L)

  expect_length(r$revdep_check_task_spec$rev.both.error$errors$issues, 1L)
  expect_true(
    grepl("Running the tests in",
          r$revdep_check_task_spec$rev.both.error$errors$issues),
    grepl("is not an exported object from",
          r$revdep_check_task_spec$rev.both.error$errors$issues)
  )
  expect_length(r$revdep_check_task_spec$rev.both.error$errors$potential_issues$new, 0L)
  expect_length(r$revdep_check_task_spec$rev.both.error$errors$potential_issues$old, 0L)


  # rev.both.ok

  expect_length(r$revdep_check_task_spec$rev.both.ok$notes$issues, 0L)
  expect_length(r$revdep_check_task_spec$rev.both.ok$notes$potential_issues$new, 0L)
  expect_length(r$revdep_check_task_spec$rev.both.ok$notes$potential_issues$old, 0L)

  expect_length(r$revdep_check_task_spec$rev.both.ok$warnings$issues, 0L)
  expect_length(r$revdep_check_task_spec$rev.both.ok$warnings$potential_issues$new, 0L)
  expect_length(r$revdep_check_task_spec$rev.both.ok$warnings$potential_issues$old, 0L)

  expect_length(r$revdep_check_task_spec$rev.both.ok$errors$issues, 0L)
  expect_length(r$revdep_check_task_spec$rev.both.ok$errors$potential_issues$new, 0L)
  expect_length(r$revdep_check_task_spec$rev.both.ok$errors$potential_issues$old, 0L)
})

test_that("check_rev_deps works for a package without release version", {

  # Ensure source installation to make sure test works also on mac and windows
  withr::with_options(list(pkgType = "source"), {
    expect_warning(design <- check_rev_deps(
      file.path(sources_new, "pkg.suggests"),
      n = 2L, repos = repo))
  })

  r <- results(design)
  expect_s3_class(r, "checked_results")
  expect_true(is.list(r))
  expect_named(r)
  expect_length(r, 1L)
  expect_length(r$check_task_spec, 1L)

  expect_length(r$check_task_spec$`pkg.none (dev)`$notes$issues, 0L)
  expect_length(r$check_task_spec$`pkg.none (dev)`$notes$potential_issues$new, 0L)
  expect_length(r$check_task_spec$`pkg.none (dev)`$notes$potential_issues$old, 0L)

  expect_length(r$check_task_spec$`pkg.none (dev)`$warnings$issues, 0L)
  expect_length(r$check_task_spec$`pkg.none (dev)`$warnings$potential_issues$new, 0L)
  expect_length(r$check_task_spec$`pkg.none (dev)`$warnings$potential_issues$old, 0L)


  expect_length(r$check_task_spec$`pkg.none (dev)`$errors$issues, 1L)
  expect_true(
    grepl("Running the tests in",
          r$check_task_spec$`pkg.none (dev)`$errors$issues),
    grepl("\"hello world\" is not TRUE",
          r$check_task_spec$`pkg.none (dev)`$errors$issues)
  )
  expect_length(r$check_task_spec$`pkg.none (dev)`$errors$potential_issues$new, 0L)
  expect_length(r$check_task_spec$`pkg.none (dev)`$errors$potential_issues$old, 0L)
})
