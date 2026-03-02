# Available Packages

Functionally Equivalent to
[`utils::available.packages()`](https://rdrr.io/r/utils/available.packages.html),
assuming `utils`'s cache doesn't expire in the middle of a top-level
call evaluation. Modifications were made so that the results for queries
with unique arguments are only called once for each top-level
expression.

## Usage

``` r
available_packages(...)
```

## Details

Though
[`utils::available.packages()`](https://rdrr.io/r/utils/available.packages.html)
will cache the `PACKAGES` index, it must still be parsed with each call.
Since this can happen hundreds of times when building a `R CMD check`
plan, this can cause a signficiant bottleneck to the responsiveness of
this package.

    system.time({ for (i in 1:10) available.packages() })
    #>    user  system elapsed
    #>   3.453   0.196   3.655

    system.time({ for (i in 1:10) available_packages() })
    #>    user  system elapsed
    #>   0.325   0.002   0.328

## Note

This *could* be removed by propagating the
[`available.packages()`](https://rdrr.io/r/utils/available.packages.html)
database matrix through all the calls that need to use it, though this
would be a sizable refactor.
