# `R6` Checks Coordinator

A stateful object that orchestrates all separate processes required to
manage installation, library setup and run `R CMD check`s in sequence.

## See also

Other checks:
[`STATUS`](https://Genentech.github.io/checked/reference/STATUS.md),
[`check_pkgs()`](https://Genentech.github.io/checked/reference/check_pkgs.md),
[`check_rev_deps()`](https://Genentech.github.io/checked/reference/check_rev_deps.md),
[`new_checker()`](https://Genentech.github.io/checked/reference/new_checker.md)

## Public fields

- `graph`:

  ([`igraph::igraph()`](https://r.igraph.org/reference/aaa-igraph-package.html))  
  A dependency graph, storing information about which dependencies are
  required prior to execution of each check task. Created with
  [`task_graph()`](https://Genentech.github.io/checked/reference/task_graph.md)

- `plan`:

  ([`data.frame()`](https://rdrr.io/r/base/data.frame.html))  
  Checks task `data.frame` which is the source of all the checks.

- `output`:

  (`character(1)`)  
  Output directory where raw results and temporary library will be
  created and stored.

## Methods

### Public methods

- [`checker$new()`](#method-checker-new)

- [`checker$active_processes()`](#method-checker-active_processes)

- [`checker$failed_tasks()`](#method-checker-failed_tasks)

- [`checker$terminate()`](#method-checker-terminate)

- [`checker$step()`](#method-checker-step)

- [`checker$start_next_task()`](#method-checker-start_next_task)

- [`checker$is_done()`](#method-checker-is_done)

- [`checker$clone()`](#method-checker-clone)

------------------------------------------------------------------------

### Method `new()`

Initialize a new check design

Use checks data.frame to generate task graph in which all dependencies
and installation order are embedded.

#### Usage

    checker$new(
      plan,
      n = 2L,
      output = file.path(tempdir(), paste(packageName(), Sys.Date(), sep = "-")),
      lib.loc = .libPaths(),
      repos = getOption("repos"),
      restore = options::opt("restore"),
      ...
    )

#### Arguments

- `plan`:

  `plan` `data.frame`.

- `n`:

  `integer` value indicating maximum number of subprocesses that can be
  simultaneously spawned when executing tasks.

- `output`:

  `character` value specifying path where the output should be stored.

- `lib.loc`:

  `character` vector with libraries allowed to be used when checking
  packages, defaults to entire .libPaths().

- `repos`:

  `character` vector of repositories which will be used when generating
  task graph and later pulling dependencies.

- `restore`:

  `logical` value, whether output directory should be unlinked before
  running checks. If `FALSE`, an attempt will me made to restore
  previous progress from the same `output`.

- `...`:

  Additional arguments unused

#### Returns

checker.

------------------------------------------------------------------------

### Method `active_processes()`

Get Active Processes list

#### Usage

    checker$active_processes()

------------------------------------------------------------------------

### Method `failed_tasks()`

Get Failed Tasks list

#### Usage

    checker$failed_tasks()

------------------------------------------------------------------------

### Method `terminate()`

Kill All Active Design Processes

Immediately terminates all the active processes.

#### Usage

    checker$terminate()

------------------------------------------------------------------------

### Method [`step()`](https://rdrr.io/r/stats/step.html)

Fill Available Processes with Tasks

#### Usage

    checker$step()

#### Returns

A logical value, indicating whether processes are actively running.

------------------------------------------------------------------------

### Method `start_next_task()`

Start Next Task

#### Usage

    checker$start_next_task()

#### Returns

A integer value, coercible to logical to indicate whether a new process
was spawned, or `-1` if all tasks have finished.

------------------------------------------------------------------------

### Method `is_done()`

Check if checks are done

Checks whether all the scheduled tasks were successfully executed.

#### Usage

    checker$is_done()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    checker$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
library(checked)
plan <- plan_checks(c(
  system.file("example_packages", "exampleBad", package = "checked"),
  system.file("example_packages", "exampleGood", package = "checked")
))

orchestrator <- checker$new(
  plan,
  n = 10,
  repos = "https://cran.r-project.org/"
)

while (!orchestrator$is_done()) {
  orchestrator$start_next_task()
}
} # }
```
