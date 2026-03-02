# Build task graph edges

Edges describe relationships between tasks. Often, this is a dependency
between packages, requiring that some package be installed before a
latter task can be executed.

## Usage

``` r
task_graph(x, repos = getOption("repos"), ...)
```

## Arguments

- x:

  a `plan` object, containing a list of related steps.

- repos:

  `repos`, as expected by
  [`tools::package_dependencies()`](https://rdrr.io/r/tools/package_dependencies.html)
  to determine package relationships.

- ...:

  params passed to helper methods.

## Value

A `data.frame` that can be used to build
[`igraph::make_graph`](https://r.igraph.org/reference/make_graph.html)
edges.

## Details

[`tools::package_dependencies()`](https://rdrr.io/r/tools/package_dependencies.html)
is used to calculate these relationships. However, the package data
returned by
[`utils::available.packages()`](https://rdrr.io/r/utils/available.packages.html),
that is used internally to determine dependencies does not know about
local or remote packages, so those are first appended to this data set
prior to calculating edges. The bulk of this function serves to join
this data.

## Examples

``` r
if (FALSE) { # \dontrun{
task_graph(plan_rev_dep_checks("."))
} # }
```
