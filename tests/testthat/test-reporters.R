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
old <- getOption("pkgType")
options(pkgType = "source")

test_that("reporter_basic_tty works as expected for pkg.none", {
  # Entire testing suite takes too long on CRAN, we need to trim it
  skip_on_cran()

  plan <- plan_rev_dep_checks(
    file.path(sources_new, "pkg.none"),
    repos = repo
  )

  design <- checker$new(
    plan,
    n = 1L,
    repos = repo,
    restore = FALSE
  )

  reporter <- reporter_basic_tty()

  expect_snapshot(
    run(design, reporter = reporter),
    # We remove the last line as it reports the time which can change
    transform = function(lines) {
      lines[-length(lines)]
    }
  )
})

test_that("reporter_basic_tty works as expected for pkg.ok.error", {
  # Entire testing suite takes too long on CRAN, we need to trim it
  skip_on_cran()

  plan <- plan_rev_dep_checks(
    file.path(sources_new, "pkg.ok.error"),
    repos = repo
  )

  design <- checker$new(
    plan,
    n = 1L,
    repos = repo,
    restore = FALSE
  )

  reporter <- reporter_basic_tty()

  expect_snapshot(
    run(design, reporter = reporter),
    # We remove the last line as it reports the time which can change
    transform = function(lines) {
      lines <- gsub("\\d+(?:\\.\\d+)?(?:ms|s|m)", "", lines)
      lines[!startsWith(lines, "ETA")]
    }
  )
})

test_that("reporter_ansi_tty works as expected for pkg.ok.error", {
  # Entire testing suite takes too long on CRAN, we need to trim it
  skip_on_cran()

  plan <- plan_rev_dep_checks(
    file.path(sources_new, "pkg.ok.error"),
    repos = repo
  )

  design <- checker$new(
    plan,
    n = 1L,
    repos = repo,
    restore = FALSE
  )
  reporter <- reporter_ansi_tty()

  expect_no_error(suppressMessages(
    capture.output(
      run(design, reporter = reporter)
    )
  ))

  expect_s3_class(reporter$buffer, "data.frame")
  expect_equal(NROW(reporter$buffer), 7)
  expect_all_true(reporter$buffer$final)
  expect_all_false(reporter$buffer$updated)
  expect_all_false(reporter$buffer$new)
})

options(pkgType = old)
