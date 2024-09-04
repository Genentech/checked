#' @import options
options::define_options(
  "tty refresh interval when reporting results in milliseconds",
  tty_tick_interval = 0.1,
  
  "character vector indicating whether R error should be thrown when issues
   are discovered when generating results. \"never\" means that no errors
   are thrown. If \"issues\" then errors are emitted only on issues, whereas
   \"potential issues\" stands for error on both issues and potential issues.",
  results_error_on = "never",
  
  "character vector indicating which packages should be included in the results.
   \"all\" means that all packages are kept. If \"issues\" then only packages 
   with issues identified, whereas \"potential_issues\" stands for keeping
   packages with both \"issues\" and \"potential_issues\".",
  results_keep = "all",
  
  "`logical` indicating whether output directory should be unlinked before
   running checks. If `FALSE`, an attempt will me made to restore previous
   progress from the same `output`",
  restore = NA,
  
  "`logical` indicating whether default R CMD check related enviromental
   variables should be appended.",
  default_check_env_variables = TRUE,

  "`logical` indicating whether default R CMD check build args should be appended.",
  default_check_build_args = TRUE,

  "`logical` indicating whether default R CMD check args should be appended.",
  default_check_args = TRUE
)

#' @eval options::as_roxygen_docs()
NULL

#' Checked Options
#' @eval options::as_params()
#' @name options_params
#'
NULL
