checked 0.2.0 
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
  
* Add `keep` parameters to the the `print.checked_results_check_task_spec`.

* Add `error_on` parameters to the the `results.check_design`.

* Add `$get_r_exit_status()` method to both check and install processes.

* Add warnings to the reporter whenever sub-processes have non-zero exit status.

checked 0.1.0
---------------------------------------------------------------
* Package released to CRAN
