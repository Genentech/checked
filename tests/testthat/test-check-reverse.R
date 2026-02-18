# CRAN does not like out subprocesses tests resulting in false positive tests 
# results
testthat::skip_on_cran()

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
<<<<<<< HEAD
    expect_no_error(design <- check_rev_deps(
      file.path(sources_new, "pkg.suggests"),
      n = 2L,
      repos = repo,
      reporter = NULL
    ))
=======
    expect_no_warning(design <- check_rev_deps(
      file.path(sources_new, "pkg.suggests"),
      n = 2L, repos = repo, env = c("NOT_CRAN" = "false", options::opt("check_envvars"))))
  })

  expect_identical(
    design$input$package[[1]]$env, 
    c("NOT_CRAN" = "false", options::opt("check_envvars"))
  )
  
  r <- results(design)
  expect_s3_class(r, "checked_results")
  expect_true(is.list(r))
  expect_named(r)
  expect_length(r, 1L)
  expect_length(r$revdep_check_task_spec, 2L)

  # pkg.none
  expect_length(r$revdep_check_task_spec$pkg.none$notes$issues, 0L)
  expect_length(r$revdep_check_task_spec$pkg.none$notes$potential_issues$new, 0L)
  expect_length(r$revdep_check_task_spec$pkg.none$notes$potential_issues$old, 0L)
  
  expect_length(r$revdep_check_task_spec$pkg.none$warnings$issues, 0L)
  expect_length(r$revdep_check_task_spec$pkg.none$warnings$potential_issues$new, 0L)
  expect_length(r$revdep_check_task_spec$pkg.none$warnings$potential_issues$old, 0L)
  
  
  expect_length(r$revdep_check_task_spec$pkg.none$errors$issues, 0L)
  expect_length(r$revdep_check_task_spec$pkg.none$errors$potential_issues$new, 0L)
  expect_length(r$revdep_check_task_spec$pkg.none$errors$potential_issues$old, 0L)
  
  
  # pkg.none.broken
  expect_length(r$revdep_check_task_spec$pkg.none.broken$notes$issues, 0L)
  expect_length(r$revdep_check_task_spec$pkg.none.broken$notes$potential_issues$new, 0L)
  expect_length(r$revdep_check_task_spec$pkg.none.broken$notes$potential_issues$old, 0L)
  
  expect_length(r$revdep_check_task_spec$pkg.none.broken$warnings$issues, 0L)
  expect_length(r$revdep_check_task_spec$pkg.none.broken$warnings$potential_issues$new, 0L)
  expect_length(r$revdep_check_task_spec$pkg.none.broken$warnings$potential_issues$old, 0L)
  
  
  expect_length(r$revdep_check_task_spec$pkg.none.broken$errors$issues, 1L)
  expect_true(
    grepl("Running the tests in",
          r$revdep_check_task_spec$pkg.none.broken$errors$issues),
    grepl("\"hello world\" is not TRUE",
          r$revdep_check_task_spec$pkg.none.broken$errors$issues)
  )
  expect_length(r$revdep_check_task_spec$pkg.none.broken$errors$potential_issues$new, 0L)
  expect_length(r$revdep_check_task_spec$pkg.none.broken$errors$potential_issues$old, 0L)
  
  # Error testing
   dir.create(temp_lib <- tempfile("testing_lib"))
   install.packages(
     file.path(sources_new, "pkg.suggests"),
     lib = temp_lib,
     type = "source",
     repos = NULL
   )
   
   withr::with_options(list(pkgType = "source"), {
     expect_error(design <- check_rev_deps(
       file.path(sources_new, "pkg.suggests"),
       lib.loc = temp_lib,
       n = 2L, repos = repo, env = c("NOT_CRAN" = "false", options::opt("check_envvars"))),
       "cannot provide accurate reverse dependency check results")
   })
})

test_that("check_dev_rev_deps works as expected", {
  withr::with_options(list(pkgType = "source"), {
    design <- check_dev_rev_deps(
      file.path(sources_new, "pkg.ok.error"),
      n = 2L, repos = repo)
>>>>>>> 3615af88add43ec6f6d21ee450e4c8433c467911
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
<<<<<<< HEAD
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
=======
  expect_length(r$check_task_spec, 2L)

  # rev.both.error
  expect_length(r$check_task_spec$`rev.both.error (dev)`$notes$issues, 0L)
  expect_length(r$check_task_spec$`rev.both.error (dev)`$warnings$issues, 1L)
  expect_true(
    grepl("Namespace in Imports field not imported from",
          r$check_task_spec$`rev.both.error (dev)`$warnings$issues),
    grepl("Missing or unexported object",
          r$check_task_spec$`rev.both.error (dev)`$warnings$issues)
  )

  expect_length(r$check_task_spec$`rev.both.error (dev)`$errors$issues, 1L)
  expect_true(
    grepl("Running the tests in",
          r$check_task_spec$`rev.both.error (dev)`$errors$issues),
    grepl("is not an exported object from",
          r$check_task_spec$`rev.both.error (dev)`$errors$issues)
  )

  # rev.both.ok
  expect_length(r$check_task_spec$`rev.both.ok (dev)`$notes$issues, 1L)
  expect_true(
    grepl("Namespace in Imports field not imported from",
          r$check_task_spec$`rev.both.ok (dev)`$notes$issues)
  )

  expect_length(r$check_task_spec$`rev.both.ok (dev)`$warnings$issues, 0L)
  expect_length(r$check_task_spec$`rev.both.ok (dev)`$errors$issues, 0L)
>>>>>>> 3615af88add43ec6f6d21ee450e4c8433c467911
})
