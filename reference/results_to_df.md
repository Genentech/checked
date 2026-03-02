# Summarize checked results as data.frame

Turns checked results into a list of data.frams, one for each meta root
task. Such form makes quick results analysis easier by providing a
general overview of identified failures.

## Usage

``` r
results_to_df(results, ...)
```

## Arguments

- results:

  checked_results object or any of the sub-objects.

- ...:

  other prameters passed to downstream functions.

## See also

Other results:
[`print.checked_results()`](https://Genentech.github.io/checked/reference/print.checked_results.md),
[`results()`](https://Genentech.github.io/checked/reference/results.md)
