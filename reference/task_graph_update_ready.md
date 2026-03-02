# Find the Next Packages Not Dependent on an Unavailable Package

While other packages are in progress, ensure that the next selected
package already has its dependencies done.

## Usage

``` r
task_graph_update_ready(
  g,
  v = V(g),
  dependencies = TRUE,
  status = STATUS$pending
)
```

## Arguments

- g:

  A dependency graph, as produced with
  [`task_graph()`](https://Genentech.github.io/checked/reference/task_graph.md).

- v:

  Names or nodes objects of packages whose satisfiability should be
  checked.

- dependencies:

  Which dependencies types should be met for a node to be considered
  satisfied.

- status:

  status name. Nodes in v fill be filtered to consists only nodes with
  that status.

## Value

The name of the next package to prioritize

## Details

There are helpers defined for particular use cases that strictly rely on
the
[`task_graph_which_ready()`](https://Genentech.github.io/checked/reference/task_graph_which_ready.md),
they are:

- `task_graph_update_check_ready()` - Updates check vertices whose all
  dependencies are satisfied.

- `task_graph_update_install_ready()` - Update install vertices whose
  all dependencies are satisfied.

- [`task_graph_which_ready()`](https://Genentech.github.io/checked/reference/task_graph_which_ready.md) -
  List vertices whose wit ready status.
