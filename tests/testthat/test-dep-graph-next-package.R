test_that("dep_graph_next_package finds next installable package", {
  # nolint start, styler: off
  g <- igraph::make_graph(~
    A +- B +- C,
    A +------ D,
    A +- E +- D,
    A +- F +- D
  )
  # nolint end, styler: on

  # initialize graph characteristics to mock dep graph
  E(g)$type <- "Depends"
  V(g)$root <- V(g)$name == "A"
  expect_silent(g <- task_graph_sort(g))
  expect_equal(V(g)$name, c("D", "C", "F", "E", "B", "A"))

  # initialize graph, such that "D" is not completed when "E"  would be next
  # by order due to "D" having a long install time
  V(g)$status <- STATUS[["pending"]]
  V(g)$type <- "install"
  V(g)["D"]$status <- STATUS[["in progress"]]
  V(g)["C"]$status <- STATUS[["done"]]
  g <- task_graph_update_ready(g)
  expect_equal(names(task_graph_which_ready(g)), "B")

  # if the order is reversed, now "F" and "E" should be next
  V(g)$status <- STATUS[["pending"]]
  V(g)["D"]$status <- STATUS[["done"]]
  V(g)["C"]$status <- STATUS[["in progress"]]
  g <- task_graph_update_ready(g)
  expect_equal(names(task_graph_which_ready(g)), c("F", "E"))
})
