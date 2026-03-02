# Libpaths from task graph

Function that traverses over the task dependency task to acquire
libpaths for given nodes. It ensures that when runing a node, a libpath
is constructed which has all the required packages on it.

## Usage

``` r
task_graph_libpaths(g, node = NULL, lib.loc = .libPaths(), output = tempdir())
```

## Arguments

- g:

  `task_graph` object

- node:

  Node(s) for which libpath should be constructed based on `g`

- lib.loc:

  Library paths, defaulting to
  [`.libPaths()`](https://rdrr.io/r/base/libPaths.html).

- output:

  Path to the checked output directory
