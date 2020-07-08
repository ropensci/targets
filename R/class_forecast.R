forecast_new <- function() {
  forecast_class$new()
}

forecast_class <- R6::R6Class(
  classname = "tar_forecast",
  inherit = reporter_class,
  class = FALSE,
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    report_start = function() {
      cli_header_outdated()
    },
    report_skipped = function(target, progress) {
    },
    report_outdated = function(checked, outdated) {
      cli_outdated(checked$count, outdated$count)
    },
    report_end = function(progress = NULL) {
      message("")
    }
  )
)
