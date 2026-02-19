library(testthat)
library(checked)

# Meet CRAN multiple core usage requirement
Sys.setenv("OMP_THREAD_LIMIT" = 2)
test_check("checked")
