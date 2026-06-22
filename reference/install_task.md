# Create a task to install a package and dependencies

Create a task to install a package and dependencies

## Usage

``` r
install_task(
  origin,
  type = package_install_type(origin),
  INSTALL_opts = NULL,
  lib = lib_path(origin),
  env = options::opt("install_envvars"),
  ...
)
```

## Arguments

- origin:

  [`pkg_origin()`](https://Genentech.github.io/checked/reference/pkg_origin.md)
  object.

- type:

  character, indicating the type of package to download and install.
  Will be `"source"` except on Windows and some macOS builds: see the
  section on ‘Binary packages’ for those.

- INSTALL_opts:

  an optional character vector of additional option(s) to be passed to
  `R CMD INSTALL` for a source package install. E.g.,
  `c("--html", "--no-multiarch", "--no-test-load")` or, for macOS,
  `"--dsym"`.

  Can also be a named list of character vectors to be used as additional
  options, with names the respective package names.

- lib:

  Any object that can be passed to
  [`lib()`](https://Genentech.github.io/checked/reference/lib.md) to
  generate a library path.

- env:

  Environment variables to set for the child process.

- ...:

  further arguments to be passed to
  [`download.file`](https://rdrr.io/r/utils/download.file.html),
  [`available.packages`](https://rdrr.io/r/utils/available.packages.html),
  or to the functions for binary installs on macOS and Windows (which
  accept an argument `"lock"`: see the section on ‘Locking’).

## See also

Other tasks:
[`check_task()`](https://Genentech.github.io/checked/reference/check_task.md),
[`meta_task()`](https://Genentech.github.io/checked/reference/meta_task.md),
[`task()`](https://Genentech.github.io/checked/reference/task.md)
