test_that("s3_example_func works", {
  expect_equal(s3_example_func(1L), "default")  
  expect_equal(s3_example_func(list(1, 2, 3)), "list")  
})
