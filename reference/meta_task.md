# Construct a 'Meta' Task

Meta tasks are tasks which are not intended to perform computation. They
exist simply to provide relationships among computational tasks.

## Usage

``` r
meta_task(..., .subclass = NULL)
```

## Arguments

- ...:

  Objects passed to specified class functions

- .subclass:

  character name of the subclass. It will be appended with "\_meta"
  suffix.

## See also

Other tasks:
[`check_task()`](https://Genentech.github.io/checked/reference/check_task.md),
[`install_task()`](https://Genentech.github.io/checked/reference/install_task.md),
[`task()`](https://Genentech.github.io/checked/reference/task.md)
