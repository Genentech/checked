# Check functions

Set of functions to run orchestrated `R CMD check`s and automatically
manage the dependencies installation. Each functions prepares the plan
based on the supplied package source(s) which includes installing
dependencies and running required `R CMD check`s. All the functions are
parallelized through sperate processes

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
