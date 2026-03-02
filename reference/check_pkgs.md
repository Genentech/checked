# Check packages

Runs classical `R CMD check` for the given source package. It first
identifies and installs, in parallel, all dependencies required to check
the package. Then, it runs `R CMD check` for each specified package.

## Usage

``` r
check_pkgs(
  package,
  n = 2L,
  output = tempfile(paste(utils::packageName(), Sys.Date(), sep = "-")),
  lib.loc = .libPaths(),
  repos = getOption("repos"),
  restore = TRUE,
  ...
)
```

## Arguments

- package:

  A path to either package, directory with packages or name of the
  package (details)

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

## See also

Other checks:
[`STATUS`](https://Genentech.github.io/checked/reference/STATUS.md),
[`check_rev_deps()`](https://Genentech.github.io/checked/reference/check_rev_deps.md),
[`checker`](https://Genentech.github.io/checked/reference/checker.md),
[`new_checker()`](https://Genentech.github.io/checked/reference/new_checker.md)
