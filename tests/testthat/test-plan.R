test_that("rev_dep_check_tasks_df works with deafult params", {
  expect_silent(
    df <- plan_rev_dep_checks(
      test_path("fixtures", "DALEXtra"),
      repos = "https://cran.r-project.org/"
    )
  )

  expect_s3_class(df, "data.frame")
  expect_true(NROW(df) >= 8)
  expect_named(df, c("alias", "version", "package", "custom"))

  expect_s3_class(df$package, "list_of_task")
  task_classes <- vcapply(df$package, function(x) class(x)[[1]])
  expect_equal(unique(task_classes), "revdep_check_task")
  pkg_sub_classes <- vcapply(df$package, function(x) class(x$package)[[1]])
  expect_equal(unique(pkg_sub_classes), "pkg_origin_repo")

  expect_s3_class(df$custom, "list_of_task")
  task_classes <- vcapply(df$custom, function(x) class(x)[[1]])
  expect_equal(unique(task_classes), "custom_install_task")
  pkg_sub_classes <- vcapply(df$custom, function(x) class(x$package)[[1]])
  expect_equal(unique(pkg_sub_classes), c("pkg_origin_local", "pkg_origin_repo"))

  expect_true(all(endsWith(df$alias[seq(1, NROW(df), by = 2)], "(dev)")))
  expect_true(all(endsWith(df$alias[seq(2, NROW(df), by = 2)], "(v2.3.0)")))

  # Test displayes
  expect_no_error(expect_output(print(df)))
  expect_no_error(expect_output(print(df$package)))
  expect_no_error(expect_output(print(df$custom)))
})

test_that("rev_dep_check_tasks_df development_only = TRUE", {
  expect_silent(
    plan <- plan_rev_dep_checks(
      test_path("fixtures", "DALEXtra"),
      repos = "https://cran.r-project.org/",
      versions = "dev"
    )
  )

  expect_s3_class(plan, "data.frame")
  expect_true(NROW(plan) >= 4)
  expect_named(plan, c("alias", "version", "package", "custom"))

  expect_s3_class(plan$package, "list_of_task")
  task_classes <- vcapply(plan$package, function(x) class(x)[[1]])
  expect_equal(unique(task_classes), "check_task")
  pkg_sub_classes <- vcapply(plan$package, function(x) class(x$package)[[1]])
  expect_equal(unique(pkg_sub_classes), "pkg_origin_repo")

  expect_s3_class(plan$custom, "list_of_task")
  task_classes <- vcapply(plan$custom, function(x) class(x)[[1]])
  expect_equal(unique(task_classes), "custom_install_task")
  package_sub_classes <- vcapply(plan$custom, function(x) class(x$package)[[1]])
  expect_equal(unique(package_sub_classes), c("pkg_origin_local"))

  expect_true(all(endsWith(plan$alias, "(dev)")))
  expect_true(all(!endsWith(plan$alias, "(v2.3.0)")))
})

test_that("source_check_tasks_df works as expected", {
  examples_path <- system.file("example_packages", package = "checked")
  expect_silent(
    df <- plan_checks(
      c(
        test_path("fixtures", "DALEXtra"),
        test_path("fixtures", "rd2markdown"),
        file.path(examples_path, "exampleGood"),
        file.path(examples_path, "exampleOkay"),
        file.path(examples_path, "exampleBad")
      )
    )
  )
  expect_s3_class(df, "data.frame")
  expect_equal(NROW(df), 5)
  expect_named(df, c("alias", "version", "package", "custom"))

  expect_s3_class(df$package, "list_of_task")
  expect_equal(unique(vcapply(df$package, function(x) class(x)[[1]])), "check_task")
  expect_equal(unique(vcapply(df$package, function(x) class(x$package)[[1]])), "pkg_origin_local")

  expect_s3_class(df$custom, "list_of_task")
  expect_equal(unique(vcapply(df$custom, function(x) class(x)[[1]])), "custom_install_task")
  expect_equal(unique(vcapply(df$custom, function(x) class(x$package)[[1]])), "NULL")

  expect_true(all(endsWith(df$alias, "(source)")))
})

test_that("source_check_tasks_df aliases are properly handled", {
  examples_path <- system.file("example_packages", package = "checked")
  names <- c(
    "DALEXtra_new",
    "rd2markdown_new",
    "exampleGood_new",
    "exampleOkay_new",
    "exampleBad_new"
  )

  path <- c(
    test_path("fixtures", "DALEXtra"),
    test_path("fixtures", "rd2markdown"),
    file.path(examples_path, "exampleGood"),
    file.path(examples_path, "exampleOkay"),
    file.path(examples_path, "exampleBad")
  )
  names(path) <- names

  expect_silent(
    df <- plan_checks(path)
  )

  expect_true(all(endsWith(df$alias, "_new")))
  expect_equal(df$alias, names)

  expect_silent(
    df <- plan_checks(c(
      file.path(examples_path, "exampleGood"),
      file.path(examples_path, "exampleGood"),
      file.path(examples_path, "exampleGood")
    ))
  )

  expect_equal(
    df$alias,
    c(
      "exampleGood (source_1)",
      "exampleGood (source_2)",
      "exampleGood (source_3)"
    )
  )
})
