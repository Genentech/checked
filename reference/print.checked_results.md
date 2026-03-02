# Print checked results

Print checked results

## Usage

``` r
# S3 method for class 'checked_results'
print(x, ...)

# S3 method for class 'rev_dep_dep_results'
print(x, ..., name = NULL, keep = options::opt("results_keep"))

# S3 method for class 'local_check_results'
print(x, ..., name = NULL, keep = options::opt("results_keep"))
```

## Arguments

- x:

  an object to be printed.

- ...:

  other parameters.

- name:

  character name of the `rev_dep_dep` package

- keep:

  character vector indicating which packages should be included in the
  results. "all" means that all packages are kept. If "issues" then only
  packages with issues identified, whereas "potential_issues" stands for
  keeping packages with both "issues" and "potential_issues". (Defaults
  to `"all"`, overwritable using option 'checked.results_keep' or
  environment variable 'R_CHECKED_RESULTS_KEEP')

## See also

Other results:
[`results()`](https://Genentech.github.io/checked/reference/results.md),
[`results_to_df()`](https://Genentech.github.io/checked/reference/results_to_df.md)
