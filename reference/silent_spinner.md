# Create a 'cli' Spinner With Suppressed Output

'cli' will implicitly push spinner output to various output streams,
affecting the terminal cursor position. To allow for a terminal
interface that has spinners above the last line, this function
suppresses the output and simply returns its frame contents.

## Usage

``` r
silent_spinner(..., stream = devnull())
```

## Arguments

- ...:

  passed to
  [cli::make_spinner](https://cli.r-lib.org/reference/make_spinner.html)

- stream:

  passed to
  [cli::make_spinner](https://cli.r-lib.org/reference/make_spinner.html),
  defaults to a null file device

## Value

A interface similar to a 'cli' spinner, but with suppressed output
