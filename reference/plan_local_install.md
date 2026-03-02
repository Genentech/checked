# Plan source package installation

Generates a plan for running installing a package from source.

## Usage

``` r
plan_local_install(package, repos = getOption("repos"))
```

## Arguments

- package:

  A path to package source.

- repos:

  repository used to identify packages when name is provided.

## See also

Other plan:
[`plan_local_checks()`](https://Genentech.github.io/checked/reference/plan_local_checks.md),
[`plan_rev_dep_checks()`](https://Genentech.github.io/checked/reference/plan_rev_dep_checks.md)
