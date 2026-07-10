# checked Options

Internally used, package-specific options. All options will prioritize R
options() values, and fall back to environment variables if undefined.
If neither the option nor the environment variable is set, a default
value is used.

## Checking Option Values

Option values specific to `checked` can be accessed by passing the
package name to `env`.

    options::opts(env = "checked")

    options::opt(x, default, env = "checked")

## Options

- tty_tick_interval:

  default:

  :   0.1

  option:

  :   checked.tty_tick_interval

  envvar:

  :   R_CHECKED_TTY_TICK_INTERVAL (evaluated if possible, raw string
      otherwise)

- tty_default_height:

  default:

  :   50

  option:

  :   checked.tty_default_height

  envvar:

  :   R_CHECKED_TTY_DEFAULT_HEIGHT (evaluated if possible, raw string
      otherwise)

- proactive_gc:

  default:

  :   TRUE

  option:

  :   checked.proactive_gc

  envvar:

  :   R_CHECKED_PROACTIVE_GC (evaluated if possible, raw string
      otherwise)

- results_error_on:

  default:

  :   "never"

  option:

  :   checked.results_error_on

  envvar:

  :   R_CHECKED_RESULTS_ERROR_ON (evaluated if possible, raw string
      otherwise)

- results_keep:

  default:

  :   "all"

  option:

  :   checked.results_keep

  envvar:

  :   R_CHECKED_RESULTS_KEEP (evaluated if possible, raw string
      otherwise)

- restore:

  default:

  :   NA

  option:

  :   checked.restore

  envvar:

  :   R_CHECKED_RESTORE (evaluated if possible, raw string otherwise)

- add_remotes:

  default:

  :   TRUE

  option:

  :   checked.add_remotes

  envvar:

  :   R_CHECKED_ADD_REMOTES (evaluated if possible, raw string
      otherwise)

- check_envvars:

  default:

  :   c(`_R_CHECK_FORCE_SUGGESTS_` = "false", `_R_CHECK_RD_XREFS_` = "false",
              `_R_CHECK_SYSTEM_CLOCK_` = "false", `_R_CHECK_SUGGESTS_ONLY_` = "true",
              `_R_CHECK_CRAN_INCOMING_` = "false")

  option:

  :   checked.check_envvars

  envvar:

  :   R_CHECKED_CHECK_ENVVARS (evaluated if possible, raw string
      otherwise)

- check_build_args:

  default:

  :   c("--no-build-vignettes", "--no-manual")

  option:

  :   checked.check_build_args

  envvar:

  :   R_CHECKED_CHECK_BUILD_ARGS (space-separated R CMD build flags)

- check_args:

  default:

  :   c("--timings", "--ignore-vignettes", "--no-manual", "--as-cran")

  option:

  :   checked.check_args

  envvar:

  :   R_CHECKED_CHECK_ARGS (space-separated R CMD check flags)

- install_envvars:

  default:

  :   callr::rcmd_safe_env()

  option:

  :   checked.install_envvars

  envvar:

  :   R_CHECKED_INSTALL_ENVVARS (evaluated if possible, raw string
      otherwise)

- install_system_profile:

  default:

  :   FALSE

  option:

  :   checked.install_system_profile

  envvar:

  :   R_CHECKED_INSTALL_SYSTEM_PROFILE (evaluated if possible, raw
      string otherwise)

- install_user_profile:

  default:

  :   "project"

  option:

  :   checked.install_user_profile

  envvar:

  :   R_CHECKED_INSTALL_USER_PROFILE (evaluated if possible, raw string
      otherwise)

- install_opts_to_inherit:

  default:

  :   list("timeout", "available_packages_filters", "HTTPUserAgent",
              "pkgType")

  option:

  :   checked.install_opts_to_inherit

  envvar:

  :   R_CHECKED_INSTALL_OPTS_TO_INHERIT (evaluated if possible, raw
      string otherwise)

## See also

options getOption Sys.setenv Sys.getenv

Other documentation:
[`options_params`](https://Genentech.github.io/checked/reference/options_params.md)
