# Run a Series of `R CMD check`s

`run()` provides a generic, and is the central interface for executing
[checker](https://Genentech.github.io/checked/reference/checker.md)s. If
a path is provided, a new reverse dependency check plan is generated
from the source code path. Otherwise a plan can be built separately and
executed using `run()`.

## Usage

``` r
run(checker, ..., reporter = reporter_default(checker))
```

## Arguments

- checker:

  `character` or `checker` If a `character` value is provided, it is
  first coerced into a `checker` using
  [`new_rev_dep_checker()`](https://Genentech.github.io/checked/reference/new_checker.md).

- ...:

  Additional arguments passed to
  [`new_rev_dep_checker()`](https://Genentech.github.io/checked/reference/new_checker.md)

- reporter:

  A reporter to provide progress updates. Will default to the most
  expressive command-line reporter given your terminal capabilities.
