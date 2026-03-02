default_env <- options::opt("check_envvars", env = "checked")
default_args <- options::opt("check_args", env = "checked")
default_build_args <- options::opt("check_build_args", env = "checked")

test_that("rev_dep_check_tasks_df works with deafult params", {
  expect_silent(
    plan <- plan_rev_dep_checks(
      test_path("fixtures", "DALEXtra"),
      repos = "https://packagemanager.posit.co/cran/2026-02-01"
    )
  )

  expect_s3_class(plan, "task_graph")
  expect_true(length(plan) >= 15)

  expect_all_true(
    c(
      "rev_dep_dep_meta_task",
      "rev_dep_check_meta_task",
      "check_task",
      "install_task"
    ) %in% vcapply(V(plan)$task, function(x) class(x)[[1]])
  )
  expect_s3_class(V(plan)$task[[1]], "rev_dep_dep_meta_task")
  expect_s3_class(V(plan)$task[[1]]$origin, "pkg_origin_local")
  expect_true(V(plan)$task[[1]]$origin$package == "DALEXtra")
  expect_s3_class(V(plan)$task[[1]]$origin$version, "package_version")
  expect_true(V(plan)$task[[1]]$origin$version == "2.3.0")

  expect_s3_class(V(plan)$task[[3]], "check_task")
  expect_equal(
    names(V(plan)$task[[3]]),
    c("env", "args", "build_args", "origin", "seed")
  )
  expect_s3_class(V(plan)$task[[3]]$origin, "pkg_origin_repo")
  expect_equal(V(plan)$task[[3]]$env, default_env)
  expect_equal(V(plan)$task[[3]]$args, default_args)
  expect_equal(V(plan)$task[[3]]$build_args, default_build_args)
  expect_true(V(plan)$task[[3]]$origin$package == "marginaleffects")
  expect_s3_class(V(plan)$task[[3]]$origin$version, "package_version")
  expect_true(V(plan)$task[[3]]$origin$version == "0.31.0")

  # Test displayes
  expect_no_error(expect_output(print(plan)))
  expect_no_error(expect_output(print(V(plan)$task[[1]])))
  expect_no_error(expect_output(print(V(plan)$task[[3]])))
  expect_no_error(expect_output(print(V(plan)$task[[3]]$origin)))
})

test_that("source_check_tasks_df works as expected", {
  examples_path <- system.file("example_packages", package = "checked")
  expect_silent(
    plan <- plan_local_checks(
      c(
        test_path("fixtures", "DALEXtra"),
        test_path("fixtures", "rd2markdown"),
        file.path(examples_path, "exampleGood"),
        file.path(examples_path, "exampleOkay"),
        file.path(examples_path, "exampleBad")
      )
    )
  )
  expect_s3_class(plan, "task_graph")
  expect_true(length(plan) == 6)

  expect_all_true(
    c(
      "local_check_meta_task",
      "check_task"
    ) %in% vcapply(V(plan)$task, function(x) class(x)[[1]])
  )
  expect_s3_class(V(plan)$task[[1]], "local_check_meta_task")
  expect_null(V(plan)$task[[1]]$origin)

  expect_s3_class(V(plan)$task[[2]], "check_task")
  expect_equal(
    names(V(plan)$task[[3]]),
    c("env", "args", "build_args", "origin")
  )
  expect_s3_class(V(plan)$task[[3]]$origin, "pkg_origin_local")
  expect_equal(V(plan)$task[[3]]$env, default_env)
  expect_equal(V(plan)$task[[3]]$args, default_args)
  expect_equal(V(plan)$task[[3]]$build_args, default_build_args)
  expect_true(V(plan)$task[[3]]$origin$package == "rd2markdown")
  expect_s3_class(V(plan)$task[[3]]$origin$version, "package_version")
  expect_true(V(plan)$task[[3]]$origin$version == "0.0.8")

  # Test displayes
  expect_no_error(expect_output(print(plan)))
  expect_no_error(expect_output(print(V(plan)$task[[1]])))
  expect_no_error(expect_output(print(V(plan)$task[[3]])))
  expect_no_error(expect_output(print(V(plan)$task[[3]]$origin)))
})
