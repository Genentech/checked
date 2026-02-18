if (require("pkg.suggests", quietly = TRUE)) {
  library(pkg.none)
  stopifnot(hello_world() == "hello world")
}

