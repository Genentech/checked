# Plan source package installation

Generates a plan for running installing a package from source.

## Usage

``` r
plan_local_install(
  package,
  repos = getOption("repos"),
  remotes_dependencies = TRUE,
  INSTALL_opts = c()
)
```

## Arguments

- package:

  A path to package source.

- repos:

  repository used to identify packages when name is provided.

- remotes_dependencies:

  A vector of length one or a named list. Compatible with
  [`as_pkg_dependencies`](https://Genentech.github.io/checked/reference/as_pkg_dependencies.md).
  Used to filter out remotes dependencies.

- INSTALL_opts:

  Options to set while the root package is being installed. Check
  [`utils::install.packages`](https://rdrr.io/r/utils/install.packages.html)
  for details.

## See also

Other plan:
[`plan_local_checks()`](https://Genentech.github.io/checked/reference/plan_local_checks.md),
[`plan_rev_dep_checks()`](https://Genentech.github.io/checked/reference/plan_rev_dep_checks.md)
