# Checked Options

Checked Options

## Arguments

- proactive_gc:

  `logical`, indicating whether additional garbage collection should be
  performed before starting a new task, if at least one process recently
  finalized. This can cause the checker to orchestrate tasks slower but
  is recommended to be used for designs with many sub-processes required
  as native garbage collection can lag leading to memory issues. Disable
  only when maximum prefromance is required and memory is not the issue.
  (Defaults to `TRUE`, overwritable using option 'checked.proactive_gc'
  or environment variable 'R_CHECKED_PROACTIVE_GC')

- results_error_on:

  character vector indicating whether R error should be thrown when
  issues are discovered when generating results. "never" means that no
  errors are thrown. If "issues" then errors are emitted only on issues,
  whereas "potential issues" stands for error on both issues and
  potential issues. (Defaults to `"never"`, overwritable using option
  'checked.results_error_on' or environment variable
  'R_CHECKED_RESULTS_ERROR_ON')

- check_args:

  `character` vector of args passed to the R CMD check. (Defaults to
  `c("--timings", "--ignore-vignettes", "--no-manual", "--as-cran")`,
  overwritable using option 'checked.check_args' or environment variable
  'R_CHECKED_CHECK_ARGS')

- results_keep:

  character vector indicating which packages should be included in the
  results. "all" means that all packages are kept. If "issues" then only
  packages with issues identified, whereas "potential_issues" stands for
  keeping packages with both "issues" and "potential_issues". (Defaults
  to `"all"`, overwritable using option 'checked.results_keep' or
  environment variable 'R_CHECKED_RESULTS_KEEP')

- add_remotes:

  `logical` indicating whether origins inheriting from
  `pkg_origin_local`, should be scanned for packages in the `remotes`
  field and added while constrocuting a plan `task_grap` (Defaults to
  `TRUE`, overwritable using option 'checked.add_remotes' or environment
  variable 'R_CHECKED_ADD_REMOTES')

- check_envvars:

  named `character` vector of environment variables to use during the R
  CMD check. (Defaults to
  `c(`*R_CHECK_FORCE_SUGGESTS*`= "false",`*R_CHECK_RD_XREFS*`= "false", ; `*R_CHECK_SYSTEM_CLOCK*`= "false",`*R_CHECK_SUGGESTS_ONLY*`= "true", ; `*R_CHECK_CRAN_INCOMING*` = "false")`,
  overwritable using option 'checked.check_envvars' or environment
  variable 'R_CHECKED_CHECK_ENVVARS')

- tty_tick_interval:

  tty refresh interval when reporting results in milliseconds (Defaults
  to `0.1`, overwritable using option 'checked.tty_tick_interval' or
  environment variable 'R_CHECKED_TTY_TICK_INTERVAL')

- check_build_args:

  `character` vector of args passed to the R CMD build. (Defaults to
  `c("--no-build-vignettes", "--no-manual")`, overwritable using option
  'checked.check_build_args' or environment variable
  'R_CHECKED_CHECK_BUILD_ARGS')

- restore:

  `logical` indicating whether output directory should be unlinked
  before running checks. If `FALSE`, an attempt will me made to restore
  previous progress from the same `output` (Defaults to `NA`,
  overwritable using option 'checked.restore' or environment variable
  'R_CHECKED_RESTORE')

- tty_default_height:

  deafult tty height used for the ANSI reporter. Used only if correct
  values could not be acquired with system('tput lines') (Defaults to
  `50`, overwritable using option 'checked.tty_default_height' or
  environment variable 'R_CHECKED_TTY_DEFAULT_HEIGHT')

## See also

Other documentation:
[`options()`](https://Genentech.github.io/checked/reference/options.md)
