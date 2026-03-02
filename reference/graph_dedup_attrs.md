# Deduplicate attributes

Primarily intended for cleaning up the result of an
[`igraph::union()`](https://r.igraph.org/reference/union.html), which
adds duplicated attributes when attributes of the same name exist in
multiple graphs. Searches for suffixes and consolidates attributes,
taking the attribute from the first non-NA value observed.

## Usage

``` r
graph_dedup_attrs(g)
```

## Arguments

- g:

  task_graph object
