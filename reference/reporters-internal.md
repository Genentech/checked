# Reporter Internal Methods

Each of the internal methods for reporters take a reporter, the check
checker object and a calling environment.

## Usage

``` r
report_sleep(reporter, checker, sleep)

# Default S3 method
report_sleep(reporter, checker, sleep = 1)

report_start_setup(reporter, checker, ..., envir = parent.frame())

report_start_checks(reporter, checker, ..., envir = parent.frame())

# Default S3 method
report_start_checks(reporter, checker, ..., envir = parent.frame())

report_status(reporter, checker, envir = parent.frame())

report_finalize(reporter, checker)

report_task(reporter, g, v)

report_step(reporter, checker)
```

## Arguments

- reporter:

  A object produced using
  [`reporters`](https://Genentech.github.io/checked/reference/reporters.md).
  Each reporter is a thin wrapper around an environment with a class
  name for dispatch. The reporter is mutable and captures any necessary
  state that needs to be tracked while reporting.

- checker:

  [`checker`](https://Genentech.github.io/checked/reference/checker.md)
  The check checker to report as it evaluates.

- sleep:

  `numeric` An interval to pause between reporter steps.

- envir:

  `environment` An environment to attach to, to leverage on-exit hooks.
