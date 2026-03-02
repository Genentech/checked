# checked

> Batch `R CMD check` management

# Running Checks

Although `checked` is broadly capable of running arbitrary sets of
`R CMD check` tasks, reverse dependency checking is one of the most
common use cases where batch `R CMD check`s are needed.

Running reverse dependency checks is as easy as

``` r
library(checked)
x <- run("/home/dev/praise", n = 4)
results(x)
```

``` R
#> # praise reverse dependency check results (1c999505a831-meta-revdeps-of-praise) 
#> 
#> testthat package R CMD check diff 
#> notes: OK 
#> warnings: OK 
#> errors: OK 
#> 
#> goodpractice package R CMD check diff 
#> notes: OK 
#> warnings: OK 
#> errors: OK
```

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

![](reference/figures/README/ansi-tty-example.svg)
