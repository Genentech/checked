# A simple alternative to `devtools::as.package`

Functionally identical to `devtools`' `as.package`, but without
interactive options for package creation.

## Usage

``` r
as.package.remotes(x)
```

## Arguments

- x:

  A package object to coerce

## Details

Function required for communicating with the `remotes` package interface

## Note

Code inspired by `devtools` `load_pkg_description` with very minor edits
to further reduce `devtools` dependencies.
