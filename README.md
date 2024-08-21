checked
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

> Batch `R CMD check` management

<!-- badges: start -->

<https://github.com/Genentech/checked/actions/workflows/R-CMD-check.yaml/badge.svg>
[![CRAN](https://img.shields.io/cran/v/checked.svg)](https://cran.r-project.org/package=checked)
[![coverage](https://codecov.io/gh/Genentech/checked/branch/main/graph/badge.svg)](https://app.codecov.io/gh/Genentech/checked/tree/main)
<!-- badges: end -->

# Running Checks

Although `checked` is broadly capable of running arbitrary sets of `R
CMD check` tasks, reverse dependency checking is one of the most common
use cases where batch `R CMD check`s are needed.

Running reverse dependency checks is as easy as

``` r
library(checked)
x <- run("/home/dev/praise")
results(x)
```

    #> # Revdep Check Task Spec 
    #> 
    #> goodpractice package R CMD check diff 
    #> notes: OK 
    #> warnings: OK 
    #> errors: OK 
    #> 
    #> testthat package R CMD check diff 
    #> notes: OK 
    #> warnings: OK 
    #> errors: OK

## Monitoring Runs

Because running many checks in parallel can be a difficult process to
monitor, capable interfaces will provide a convenient output for
tracking various runs, check results and package installations; keeping
a log of any issues that might arise during the process.

If your editor doesn’t support the full output, you might consider
launching your checks in a terminal which should be less constrained.
This would also free up your preferred editor for you to use while your
checks run.

``` r
library(checked)
run("/home/dev/praise")
```

<picture>
<source media="(prefers-color-scheme: dark)" srcset="man/figures/README/ansi-tty-example-dark.svg">
<img src="man/figures/README/ansi-tty-example.svg" width="100%" />
</picture>
