# Task formatter bindings

This bit of code is intended for use with
[`fmt()`](https://Genentech.github.io/checked/reference/fmt.md), and
allows for us to layer symbol bindings on top of the environment used
for string interpolation which provide syntactic sugar for common
formatting components.

## Usage

``` r
task_formats(g = NULL, nodes = V(g), task = NULL, tasks = list(task))
```

## Arguments

- g:

  task_graph object.

- nodes:

  graph nodes to format.

- task:

  task to format.

- tasks:

  currently unused.
