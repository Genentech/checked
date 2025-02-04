# checked 0.2.7

* Fix a bug where wrong lib.loc was used to derive whether a package has already
  been satisfied.
  
# checked 0.2.6

* Save minimal version required by each edge and use them to properly identify
  whether dependecy is satisfied when calling `task_graph_update_done`

# checked 0.2.5

* Refine reverse suggested dependecy strategy.

# checked 0.2.4

* Fix check processes hanging forever in some system configurations.

* Remove all whitespaces (`"[[:space:]]"`) before comparing potential issues
  to skip most of the false positives.

* Shortened many user-facing function names. In general, uses of `reverse`
  were shortened to `rev`, `dependencies` was shortened to `deps`,
  `development` was shortened to `dev` and `package` was shortened to `pkg`.
  For example, `check_reverse_dependencies_development()` was shortened to
  `check_dev_rev_deps()` (@dgkf-roche)

* Fix names in the `enum` function for R lower than 4.3.

* Add tests for the reverse dependency check use case.

* Make `install_packages_process` capture session's `available_packages_filters`
  and reuse them when installing packages to ensure consistency with the main
  session filtering.

* Prettify output by stripping excessive new lines.

* `checked` now depends on `options`

* Expose `...` allowing customization of check subprocesses when creating checks df.

* Force garbage collection before scheduling task, to make sure any already
  finished processes are removed from the memory.

# checked 0.2.3

* Use custom `checked` `finisher`'s instead of the `processx` `finalizer`'s
  when cleaning up finished processes to avoid executing callbacks when
  objects are garbage collected.
  
* In `rev_dep_check_tasks_df` add custom package value for each check associated
  with the release version of the package to make sure, the current release
  version of the package is always fetched and installed.
  
* Prevent `results` from attempting to read results for unfinished checks.

# checked 0.2.2

* Clear some of the unused utils functions (`can_symlink`, `symlink_or_copy`).

* Make sure output directory is always created.

# checked 0.2.1 

* Add `results_to_file` function.

# checked 0.2.0 

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

# checked 0.1.0

* Package released to CRAN
