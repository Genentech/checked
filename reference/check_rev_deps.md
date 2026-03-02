# Check reverse dependencies

Check a package's reverse dependencies in order to identify differences
in reverse dependency check results when run alongside your package's
development and release versions.

## Usage

``` r
check_rev_deps(
  path,
  n = 2L,
  output = tempfile(paste(utils::packageName(), Sys.Date(), sep = "-")),
  lib.loc = .libPaths(),
  repos = getOption("repos"),
  reverse_repos = repos,
  restore = TRUE,
  ...
)
```

## Arguments

- path:

  file path to the package source directory

- n:

  `integer` value indicating maximum number of subprocesses that can be
  simultaneously spawned when executing tasks.

- output:

  `character` value specifying path where the output should be stored.

- lib.loc:

  `character` vector with libraries allowed to be used when checking
  packages, defaults to entire
  [`.libPaths()`](https://rdrr.io/r/base/libPaths.html).

- repos:

  `character` vector of repositories which will be used when generating
  task graph and later pulling dependencies.

- reverse_repos:

  `character` vector of repositories which will be used to pull sources
  for reverse dependencies. In some cases, for instance using binaries
  on Linux, we want to use different repositories when pulling sources
  to check and different when installing dependencies.

- restore:

  `logical` indicating whether output directory should be unlinked
  before running checks. If `FALSE`, an attempt will me made to restore
  previous progress from the same `output`

- ...:

  Additional arguments passed to
  [`run()`](https://Genentech.github.io/checked/reference/run.md)

## Value

[`checker()`](https://Genentech.github.io/checked/reference/checker.md)
R6 class storing all the details regarding checks that run. Can be
combined with
[`results`](https://Genentech.github.io/checked/reference/results.md)
and [`summary()`](https://rdrr.io/r/base/summary.html) methods to
generate results.

## Details

Runs classical reverse dependency checks for the given source package.
It first identifies reverse dependencies available in `repos`. Then,
after installing all required dependencies, runs `R CMD check` twice for
each package, one time with the release version of the given source
package installed from `repos` and a second time with the development
version installed from local source. Both `R CMD checks` are later
compared to identify changes in reverse dependency behaviors.

## See also

Other checks:
[`STATUS`](https://Genentech.github.io/checked/reference/STATUS.md),
[`check_pkgs()`](https://Genentech.github.io/checked/reference/check_pkgs.md),
[`checker`](https://Genentech.github.io/checked/reference/checker.md),
[`new_checker()`](https://Genentech.github.io/checked/reference/new_checker.md)
