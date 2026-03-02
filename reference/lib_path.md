# Make a Library Location

A description of where packages should be installed. This object
provides necessary information to determine where a package should be
installed. lib_path method creates default path handlers for given pkg
origin while lib_path_x creates an actual object.

## Usage

``` r
lib_path(x, ..., .class = c())

lib_path_default(.class = c())

lib_path_isolated(.class = c())
```

## Arguments

- x:

  A
  [`pkg_origin()`](https://Genentech.github.io/checked/reference/pkg_origin.md)
  object used for default dispatch.

- ...:

  Additional values

- .class:

  An optional subclass, used primarily for dispatch.

## See also

Other specs:
[`pkg_origin()`](https://Genentech.github.io/checked/reference/pkg_origin.md)
