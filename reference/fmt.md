# Produce cli output for a task

Provided a task, allows for use of a handful of shorthand symbols which
will use the task as a context for formatting task fields.

## Usage

``` r
fmt(..., g, nodes, task = NULL, .envir = parent.frame(), ansi = TRUE)
```

## Arguments

- ...:

  params passed to
  [`cli::format_inline`](https://cli.r-lib.org/reference/format_inline.html).

- g:

  task_graph object.

- nodes:

  graph nodes to format.

- task:

  task to format.

- .envir:

  output environment.

- ansi:

  logical whether ansi should be stripped.
