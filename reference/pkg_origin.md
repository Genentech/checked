# Package specification

Create package specification list which consists of all the details
required to identify and acquire source of the package.

## Usage

``` r
pkg_origin(package, ..., .class = c())

pkg_origin_repo(package, repos, ...)

pkg_origin_is_base(package, ...)

pkg_origin_base(package, ...)

pkg_origin_unknown(package, ...)

pkg_origin_local(path = NULL, ...)

pkg_origin_remote(remote = NULL, ...)

pkg_origin_archive(path = NULL, ...)
```

## Arguments

- package:

  name of the package.

- ...:

  parameters passed to downstream constructors.

- .class:

  Additional subclasses.

- repos:

  repository where package with given name should identified.

- path:

  path to the source of the package (either bundled or not). URLs are
  acceptable.

- remote:

  remote object from the `remotes` package used to identify non-standard
  packages.

## See also

Other specs:
[`lib_path()`](https://Genentech.github.io/checked/reference/lib_path.md)
