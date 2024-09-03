#' @import options
options::define_options(
  "tty refresh interval when reporting results in miliseconds",
  tty_tick_interval = 100,
  
  "character vector indicating whether R error should be thrown when issues
   are discovered when generating results. \"never\" means that no errors
   are thrown. If \"issues\" then errors are emitted only on issues, whereas
   \"potential issues\" stands for error on both issues and potential issues.",
  error_on = "never",
  
  "character vector indicating which packages should be included in the results.
   \"all\" means that all packages are kept. If \"issues\" then only packages 
   with issues identified, whereas \"potential_issues\" stands for keeping
   packages with both \"issues\" and \"potential_issues\".",
  keep = "all",
  
  "`logical` indicating whether output directory should be unlinked before
   running checks. If `FALSE`, an attempt will me made to restore previous
   progress from the same `output`",
  restore = NA,
  
  "`logical` indicating whether default R CMD check variables, build args or
   check args should be appended whenever custom user ones were provided.",
  add_default_configuration = TRUE
)

#' @eval options::as_roxygen_docs()
NULL

#' Checked Options
#' @eval options::as_params()
#' @name options_params
#'
NULL
