# Build Package Dependencies Table

Inspired by
[`tools::package_dependencies`](https://rdrr.io/r/tools/package_dependencies.html),
but with the added benefit of recording the dependency type and
relationships throughout the dependency tree.

## Usage

``` r
pkg_dependencies(
  packages,
  dependencies = TRUE,
  db = available_packages(),
  verbose = FALSE
)
```

## Arguments

- packages:

  a character vector of package names.

- dependencies:

  A `logical` scalar, `character` string of `"all"`, `"most"`, `"hard"`
  or `"soft"`, `NA` or a vector of dependency types compatible with
  [`as_pkg_dependencies()`](https://Genentech.github.io/checked/reference/as_pkg_dependencies.md)
  function.

- db:

  character matrix as from
  [`available.packages()`](https://rdrr.io/r/utils/available.packages.html)
  (with the default `NULL` the results of this call) or data frame
  variants thereof. Alternatively, a package database like the one
  available from <https://cran.r-project.org/web/packages/packages.rds>.

- verbose:

  logical indicating if output should monitor the package search cycles.
