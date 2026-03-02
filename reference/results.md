# Check results

Get R CMD check results

## Usage

``` r
results(x, ...)

# S3 method for class 'checker'
results(x, error_on = options::opt("results_error_on"), ...)

# S3 method for class 'integer'
results(x, checker_obj, ...)

# S3 method for class 'igraph.vs'
results(x, ...)

# S3 method for class 'rev_dep_dep_meta_task'
results(x, checker_obj, ...)

# S3 method for class 'rev_dep_check_meta_task'
results(x, checker_obj, ...)

# S3 method for class 'local_check_meta_task'
results(x, checker_obj, ...)
```

## Arguments

- x:

  object which results should be presented.

- ...:

  other parameters.

- error_on:

  character vector indicating whether R error should be thrown when
  issues are discovered when generating results. "never" means that no
  errors are thrown. If "issues" then errors are emitted only on issues,
  whereas "potential issues" stands for error on both issues and
  potential issues. (Defaults to `"never"`, overwritable using option
  'checked.results_error_on' or environment variable
  'R_CHECKED_RESULTS_ERROR_ON')

- checker_obj:

  [`checker`](https://Genentech.github.io/checked/reference/checker.md)
  object.

## See also

Other results:
[`print.checked_results()`](https://Genentech.github.io/checked/reference/print.checked_results.md),
[`results_to_df()`](https://Genentech.github.io/checked/reference/results_to_df.md)
