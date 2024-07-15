default_tty_tick_interval <- function() {
  # refresh interval in milliseconds
  getOption(paste0(utils::packageName(), ".tty_tick_interval"), 100) / 1000
}
