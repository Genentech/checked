# Get Library Location

Get Library Location

## Usage

``` r
lib(x, ...)

# S3 method for class '`NULL`'
lib(x, ...)

# S3 method for class 'character'
lib(x, ...)

# S3 method for class 'lib_path_isolated'
lib(
  x,
  ...,
  lib.root = tempdir(),
  dir_hash = hash(Sys.time(), n = 8),
  name = ""
)

# S3 method for class 'lib_path_default'
lib(x, ..., lib.loc = .libPaths())

# S3 method for class 'task'
lib(x, ...)

# S3 method for class 'install_task'
lib(x, ...)

# S3 method for class 'check_task'
lib(x, ...)
```

## Arguments

- x:

  An object describing a library location

- ...:

  additional parameters passed to methods

- lib.root:

  A root directory for the isolated library.

- dir_hash:

  unique identifier of the isolated library

- name:

  human-readable subname of the isolated library

- lib.loc:

  Library paths, defaulting to
  [`.libPaths()`](https://rdrr.io/r/base/libPaths.html).
