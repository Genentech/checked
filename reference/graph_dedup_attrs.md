# Deduplicate attributes

Primarily intended for cleaning up the result of an
[`igraph::union()`](https://r.igraph.org/reference/union.html), which
adds duplicated attributes when attributes of the same name exist in
multiple graphs. Searches for suffixes and consolidates attributes,
taking the attribute from the first non-NA value observed. The function
rebuilds the graph from scratch as accessing attributes once, operating
on lists and then assigning them to a new graph is significantly faster
than manipulating attribiutes of the existing graph.

## Usage

``` r
graph_dedup_attrs(g)
```

## Arguments

- g:

  task_graph object
