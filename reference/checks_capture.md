# Parse R CMD checks from a partial check output string

Parse R CMD checks from a partial check output string

## Usage

``` r
checks_capture(x)
```

## Arguments

- x:

  A string, compsoed of any subsection of R CMD check console output

## Value

A matrix of matches and capture groups "check" and "status" ("OK",
"NONE", "NOTE", "WARNING" or "ERROR").

## Examples

``` r
if (FALSE) { # \dontrun{
check_output <- "
* checking check one ... OK
* checking check two ... NOTE
* checking tests ...
  Running test_abc.R
  Running test_xyz.R
 NONE
* checking check three ... WARNING
* ch
"

checks_capture(check_output)
} # }
```
