% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/task_graph.R
\name{task_graph_which_satisfied}
\alias{task_graph_which_satisfied}
\title{Find the Next Packages Not Dependent on an Unavailable Package}
\usage{
task_graph_which_satisfied(
  g,
  v = igraph::V(g),
  dependencies = TRUE,
  status = STATUS$pending
)
}
\arguments{
\item{g}{A dependency graph, as produced with \code{\link[=task_graph_create]{task_graph_create()}}.}

\item{v}{Names or nodes objects of packages whose satisfiability should be
checked.}

\item{dependencies}{Which dependencies types should be met for a node to be
considered satisfied.}

\item{status}{status name. Nodes in v fill be filtered to consists only nodes
with that status.}
}
\value{
The name of the next package to prioritize
}
\description{
While other packages are in progress, ensure that the next selected package
already has its dependencies done.
}
\details{
There are helpers defined for particular use cases that strictly rely on the
\code{\link[=task_graph_which_satisfied]{task_graph_which_satisfied()}}, they are:
\itemize{
\item \code{task_graph_which_satisfied_strong()} - List vertices whose strong
dependencies are satisfied.
\item \code{task_graph_which_check_satisfied()} - List root vertices whose all
dependencies are satisfied.
\item \code{task_graph_which_install_satisfied()} - List install vertices whose
dependencies are all satisfied
}
}
\keyword{internal}
