# Start a new task

Starts task based on the `task` object encapsulated in the `node` taken
from then `task_graph` `g`. It returns an `install_process` or
`check_process` `R6` object.

## Usage

``` r
start_task(node, g, ...)
```

## Arguments

- node:

  Node(s) for which libpath should be constructed based on `g`

- g:

  `task_graph` object

- ...:

  additional params passed to downstream methods
