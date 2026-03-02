# Find nodes with ready state

List nodes which have ready state prioritizing check task nodes over
install task nodes.

## Usage

``` r
task_graph_which_ready(g)
```

## Arguments

- g:

  A dependency graph, as produced with
  [`task_graph()`](https://Genentech.github.io/checked/reference/task_graph.md).

## Value

The names of packages with ready state.
