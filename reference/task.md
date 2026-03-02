# Task specification

Create task specification list which consists of all the details
required to run specific task.

## Usage

``` r
task(..., .subclass = NULL)
```

## Arguments

- ...:

  parameters passed to downstream constructors.

- .subclass:

  Additional subclasses.

## Details

Tasks can be nested, representing either a singular task, or a set of
related tasks.

## See also

Other tasks:
[`check_task()`](https://Genentech.github.io/checked/reference/check_task.md),
[`install_task()`](https://Genentech.github.io/checked/reference/install_task.md),
[`meta_task()`](https://Genentech.github.io/checked/reference/meta_task.md)
