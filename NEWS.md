checked 0.1.0.9000 (development)
---------------------------------------------------------------
* Change default private `lib.loc` value in `check_process` R6 class
  to NULL to avoid staged installation failures.
  
* Terminate all the subprocesses if the `run` function is interrupted to
  comply with CRAN requirements.
  
* Reorganize methods in the `check_design` making some of the private.

* Remove warning about possible problems with isolation.

* Rename `development_only` parameter to `versions` in the `rev_dep_check_tasks_df`

* Make sure `check_reverse_dependencies` works when package is not available
  in the remote sources or has no reverse dependencies.

checked 0.1.0
---------------------------------------------------------------
* Package released to CRAN
