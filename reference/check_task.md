# Create a task to run `R CMD check`

Create a task to run `R CMD check`

## Usage

``` r
check_task(build_args = NULL, args = NULL, env = NULL, ...)
```

## Arguments

- build_args:

  Character vector of arguments to pass to `R CMD build`. Pass each
  argument as a single element of this character vector (do not use
  spaces to delimit arguments like you would in the shell). For example,
  `build_args = c("--force", "--keep-empty-dirs")` is a correct usage
  and `build_args = "--force --keep-empty-dirs"` is incorrect.

- args:

  Character vector of arguments to pass to `R CMD check`. Pass each
  argument as a single element of this character vector (do not use
  spaces to delimit arguments like you would in the shell). For example,
  to skip running of examples and tests, use
  `args = c("--no-examples", "--no-tests")` and not
  `args = "--no-examples --no-tests"`. (Note that instead of the
  `--output` option you should use the `check_dir` argument, because
  `--output` cannot deal with spaces and other special characters on
  Windows.)

- env:

  A named character vector, extra environment variables to set in the
  check process.

- ...:

  Arguments passed on to
  [`task`](https://Genentech.github.io/checked/reference/task.md)

  `.subclass`

  :   Additional subclasses.

## See also

Other tasks:
[`install_task()`](https://Genentech.github.io/checked/reference/install_task.md),
[`meta_task()`](https://Genentech.github.io/checked/reference/meta_task.md),
[`task()`](https://Genentech.github.io/checked/reference/task.md)
