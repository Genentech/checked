checked 0.2.3 (development)
---------------------------------------------------------------
* Fix check processes hanging forever in some system configurations.

* Remove all whitespaces (`"[[:space:]]"`) before comparing potential issues
  to skip most of the false positives.

checked 0.2.3
---------------------------------------------------------------
* Use custom `checked` `finisher`'s instead of the `processx` `finalizer`'s
  when cleaning up finished processes to avoid executing callbacks when
  objects are garbage collected.
  
* In `rev_dep_check_tasks_df` add custom package value for each check associated
  with the release version of the package to make sure, the current release
  version of the package is always fetched and installed.
  
* Prevent `results` from attempting to read results for unfinished checks.

checked 0.2.2
---------------------------------------------------------------
* Clear some of the unused utils functions (`can_symlink`, `symlink_or_copy`).

* Make sure output directory is always created.

checked 0.2.1 
---------------------------------------------------------------
* Add `results_to_file` function.

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

* Fix race condition when reporting progress for check processes.

checked 0.1.0
---------------------------------------------------------------
* Package released to CRAN
