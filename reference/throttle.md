# Generate A Rate Limiting Throttle Function

Generate A Rate Limiting Throttle Function

## Usage

``` r
throttle(interval = 0.2)
```

## Arguments

- interval:

  An interval (in seconds) that is the minimum interval before throttle
  will return `TRUE`.

## Value

A throttling function with the provided interval. When called, returns a
logical value indicating whether the throttle interval has passed (TRUE
if the interval has not yet passed).
