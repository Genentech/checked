examples_path <- system.file("example_packages", package = "checked")
names <- c("DALEXtra_new", "rd2markdown_new", "exampleGood_new", "exampleOkay_new", "exampleBad_new")
path <- path_broken <- c(
  test_path("testing_pkgs", "DALEXtra"),
  test_path("testing_pkgs", "rd2markdown"),
  file.path(examples_path, "exampleGood"),
  file.path(examples_path, "exampleOkay"),
  file.path(examples_path, "exampleBad")
)
names(path_broken) <- names

test_that("rev_dep_check_tasks_df works with deafult params", {
  expect_silent(
    df <- rev_dep_check_tasks_df(
      test_path("testing_pkgs", "DALEXtra"),
      repos = "https://cran.r-project.org/"
    )
  )
  expect_s3_class(df, "data.frame")
  expect_true(NROW(df) >= 8)
  expect_named(df, c("alias", "version", "package", "custom"))

  expect_s3_class(df$package, "list_of_task_spec")
  expect_equal(unique(vcapply(df$package, function(x) class(x)[[1]])), "revdep_check_task_spec")
  expect_equal(unique(vcapply(df$package, function(x) class(x$package_spec)[[1]])), "package_spec")

  expect_s3_class(df$custom, "list_of_task_spec")
  expect_equal(unique(vcapply(df$custom, function(x) class(x)[[1]])), "custom_install_task_spec")
  expect_equal(unique(vcapply(df$custom, function(x) class(x$package_spec)[[1]])), c("package_spec_source", "package_spec"))

  expect_true(all(endsWith(df$alias[seq(1, NROW(df), by = 2)], "(dev)")))
  expect_true(all(endsWith(df$alias[seq(2, NROW(df), by = 2)], "(v2.3.0)")))

  # Test displayes
  expect_no_error(print(df))
  expect_no_error(print(df$package))
  expect_no_error(print(df$custom))
})

test_that("task_df functions can specify subprocesses configuration", {
  expect_silent(
    df <- rev_dep_check_tasks_df(
      test_path("testing_pkgs", "DALEXtra"),
      repos = "https://cran.r-project.org/",
      env = c("NOT_CRAN" = "false"),
      args = c("--some-option", "--other-option"),
      build_args = c("--yet-another-option")
    )
  )
  
  expect_identical(
    df$package[[1]]$env, 
    c("NOT_CRAN" = "false", DEFAULT_CHECK_ENV_VARIABLES)
  )
  expect_identical(
    df$package[[1]]$args, 
    c("--some-option", "--other-option", DEFAULT_CHECK_ARGS)
  )
  expect_identical(
    df$package[[1]]$build_args, 
    c("--yet-another-option", DEFAULT_CHECK_BUILD_ARGS)
  )

  withr::with_envvar(
    c(R_CHECKED_DEFAULT_CHECK_ENV_VARIABLES = "FALSE",
      R_CHECKED_DEFAULT_CHECK_ARGS = "FALSE",
      R_CHECKED_DEFAULT_CHECK_BUILD_ARGS = "FALSE"), {
        expect_silent(
          df <- rev_dep_check_tasks_df(
            test_path("testing_pkgs", "DALEXtra"),
            repos = "https://cran.r-project.org/",
            env = c("NOT_CRAN" = "false"),
            args = c("--some-option", "--other-option"),
            build_args = c("--yet-another-option")
          )
        )
      }
  )
  
  expect_identical(
    df$package[[1]]$env, 
    c("NOT_CRAN" = "false")
  )
  expect_identical(
    df$package[[1]]$args, 
    c("--some-option", "--other-option")
  )
  expect_identical(
    df$package[[1]]$build_args, 
    "--yet-another-option"
  )
  
  withr::with_envvar(
    c(R_CHECKED_DEFAULT_CHECK_BUILD_ARGS = "FALSE"), {
        expect_silent(
          df <- source_check_tasks_df(
            path,
            env = c("NOT_CRAN" = "false"),
            args = c("--some-option", "--other-option"),
            build_args = c("--yet-another-option")
          )
        )
      }
  )
  
  expect_identical(
    df$package[[1]]$env, 
    c("NOT_CRAN" = "false", DEFAULT_CHECK_ENV_VARIABLES)
  )
  expect_identical(
    df$package[[1]]$args, 
    c("--some-option", "--other-option", DEFAULT_CHECK_ARGS)
  )
  expect_identical(
    df$package[[1]]$build_args, 
    "--yet-another-option"
  )
  
})

test_that("rev_dep_check_tasks_df development_only = TRUE", {
  expect_silent(
    df <- rev_dep_check_tasks_df(
      test_path("testing_pkgs", "DALEXtra"),
      repos = "https://cran.r-project.org/",
      versions = "dev"
    )
  )
  expect_s3_class(df, "data.frame")
  expect_true(NROW(df) >= 4)
  expect_named(df, c("alias", "version", "package", "custom"))

  expect_s3_class(df$package, "list_of_task_spec")
  expect_equal(unique(vcapply(df$package, function(x) class(x)[[1]])), "check_task_spec")
  expect_equal(unique(vcapply(df$package, function(x) class(x$package_spec)[[1]])), "package_spec")

  expect_s3_class(df$custom, "list_of_task_spec")
  expect_equal(unique(vcapply(df$custom, function(x) class(x)[[1]])), "custom_install_task_spec")
  expect_equal(unique(vcapply(df$custom, function(x) class(x$package_spec)[[1]])), "package_spec_source")

  expect_true(all(endsWith(df$alias, "(dev)")))
  expect_true(all(!endsWith(df$alias, "(v2.3.0)")))
})

test_that("source_check_tasks_df works as expected", {
  expect_silent(
    df <- source_check_tasks_df(path)
  )
  expect_s3_class(df, "data.frame")
  expect_equal(NROW(df), 5)
  expect_named(df, c("alias", "version", "package", "custom"))

  expect_s3_class(df$package, "list_of_task_spec")
  expect_equal(unique(vcapply(df$package, function(x) class(x)[[1]])), "check_task_spec")
  expect_equal(unique(vcapply(df$package, function(x) class(x$package_spec)[[1]])), "package_spec_source")

  expect_s3_class(df$custom, "list_of_task_spec")
  expect_equal(unique(vcapply(df$custom, function(x) class(x)[[1]])), "custom_install_task_spec")
  expect_equal(unique(vcapply(df$custom, function(x) class(x$package_spec)[[1]])), "NULL")

  expect_true(all(endsWith(df$alias, "(source)")))
})

test_that("source_check_tasks_df aliases are properly handled", {
  expect_silent(
    df <- source_check_tasks_df(path_broken)
  )

  expect_true(all(endsWith(df$alias, "_new")))
  expect_equal(df$alias, names)

  expect_silent(
    df <- source_check_tasks_df(c(
      file.path(examples_path, "exampleGood"),
      file.path(examples_path, "exampleGood"),
      file.path(examples_path, "exampleGood")
    ))
  )

  expect_equal(
    df$alias, c("exampleGood (source_1)", "exampleGood (source_2)", "exampleGood (source_3)")
  )
})
