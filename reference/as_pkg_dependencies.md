# Convert a value to a set of dependency types

Implements conventions established by both
[`tools::package_dependencies()`](https://rdrr.io/r/tools/package_dependencies.html)
and `pkgdepends::as_pkg_dependencies()`, following
`pkgdepends::as_pkg_dependencies()` return type structure of a list
including `$direct` and `$indirect` dependency types. Reimplemented to
avoid dependence on `pkgdepends` compilation requirements.

## Usage

``` r
as_pkg_dependencies(x)
```

## Arguments

- x:

  A `logical` scalar, `character` string of `"all"`, `"most"`, `"hard"`
  or `"soft"`, `NA` or a vector of dependency types.

## Note

locally defined and bespoke dispatch system to avoid registering methods
when loaded in combination with `pkgdepends`
