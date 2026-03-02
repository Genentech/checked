# Check checker Runner Reporters

Reporters are used to configure how output is communicated while running
a [`checker`](https://Genentech.github.io/checked/reference/checker.md).
They range from glossy command-line tools intended for displaying
progress in an interactive R session, to line-feed logs which may be
better suited for automated execution, such as in continuous itegration.

## Usage

``` r
reporter_ansi_tty()

reporter_ansi_tty2()

reporter_basic_tty()

reporter_default()
```

## Details

### `reporter_default()`

Automatically chooses an appropriate reporter based on the calling
context.

### `reporter_ansi_tty()`

Highly dynamic output for fully capable terminals. Requires multi-line
dynamic output, which may not be available in editors that that present
a terminal as a web component.

### `reporter_basic_tty()`

A line-feed reporter presenting output one line at a time, providing a
reporter with minimal assumptions about terminal capabilities.
