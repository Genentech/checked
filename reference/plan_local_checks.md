# Plan R CMD Checks

Generates a plan for running R CMD check for a specified set of
packages.

## Usage

``` r
plan_local_checks(package, repos = getOption("repos"))
```

## Arguments

- package:

  A path to either package, directory with packages or name of the
  package (details)

- repos:

  repository used to identify packages when name is provided.

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
