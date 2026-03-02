# Sort Task Graph by Strong Dependency Order

Sort Task Graph by Strong Dependency Order

## Usage

``` r
task_graph_sort(g)
```

## Arguments

- g:

  A [igraph::graph](https://r.igraph.org/reference/graph.html), expected
  to contain node attribute `type`.

## Value

The [igraph::graph](https://r.igraph.org/reference/graph.html) `g`, with
vertices sorted in preferred installation order.

## Note

Cyclic dependencies are possible. Cyclic dependencies are disallowed for
all hard dependencies on CRAN today, though there have been historical
instances where they appeared on CRAN.

Installation priority is based on:

1.  Total dependency footprint (low to high)

2.  Topology (leaf nodes first)
