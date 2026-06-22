# Plan R CMD Checks

Generates a plan for running R CMD check for a specified set of
packages.

## Usage

``` r
plan_local_checks(
  package,
  repos = getOption("repos"),
  remotes_dependencies = TRUE
)
```

## Arguments

- package:

  A path to either package, directory with packages or name of the
  package (details)

- repos:

  repository used to identify packages when name is provided.

- remotes_dependencies:

  A vector of length one or a named list. Compatible with
  [`as_pkg_dependencies`](https://Genentech.github.io/checked/reference/as_pkg_dependencies.md).
  Used to filter out remotes dependencies.

## Details

`package` parameter has two different allowed values:

- Package - checked looks for a DESCRIPTION file in the provided path,
  if found treats it like a source package.

- If the specified value does not correspond to a source package, the
  parameter is treated as the name and `repos` parameter is used to
  identify the source.

## See also

Other plan:
[`plan_local_install()`](https://Genentech.github.io/checked/reference/plan_local_install.md),
[`plan_rev_dep_checks()`](https://Genentech.github.io/checked/reference/plan_rev_dep_checks.md)
