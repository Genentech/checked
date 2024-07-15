test_that("S4Example names", {
  expect_silent(s4ex <- S4Example(data = list(a = 1, b = 2)))
  expect_equal(names(s4ex), c("a", "b"))
})
