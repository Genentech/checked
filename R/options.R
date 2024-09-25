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
  
  "named `character` vector of environment variables to use during R CMD check.",
  check_envvars = c(
    "_R_CHECK_FORCE_SUGGESTS_" = FALSE,
    "_R_CHECK_RD_XREFS_" = FALSE,
    "_R_CHECK_SYSTEM_CLOCK_" = FALSE,
    "_R_CHECK_SUGGESTS_ONLY_" = TRUE
  ),

  "`character` vector of args passed to the R CMD build.",
  check_build_args = c(
    "--no-build-vignettes",
    "--no-manual"
  ),

  "`character` vector of args passed to the R CMD check.",
  check_args = c(
    "--timings",
    "--ignore-vignettes",
    "--no-manual"
  )
)

#' @eval options::as_roxygen_docs()
NULL

#' Checked Options
#' @eval options::as_params()
#' @name options_params
#'
NULL
