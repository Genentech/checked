# Plan Reverse Dependency Checks

Generates a plan for running reverse dependency check for certain source
package. In such case `path` should be proivded with a directory path to
the development version of the package and `repos` should be a
repository for which reverse dependencies should be identified.

## Usage

``` r
plan_rev_dep_checks(path, repos = getOption("repos"))
```

## Arguments

- path:

  path to the package source.

- repos:

  repository used to identify reverse dependencies.

## See also

Other plan:
[`plan_local_checks()`](https://Genentech.github.io/checked/reference/plan_local_checks.md),
[`plan_local_install()`](https://Genentech.github.io/checked/reference/plan_local_install.md)
