forecast_new <- function(seconds_interval = NULL) {
  forecast_class$new(seconds_interval = seconds_interval)
}

forecast_class <- R6::R6Class(
  classname = "tar_forecast",
  inherit = reporter_class,
  class = FALSE,
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    flush_messages = function() {
      if (!is.null(self$buffer)) {
        message(self$buffer, appendLF = FALSE)
        self$buffer <- NULL
      }
    },
    report_outdated = function(outdated) {
      now <- time_seconds_local()
      if ((now - seconds_flushed) > seconds_interval) {
        self$buffer <- cli_forecast(outdated$cli_data(), print = FALSE)
        flush_messages()
        self$seconds_flushed <- now
      }
    },
    report_outdated_end = function(outdated) {
      self$buffer <- cli_forecast(outdated$cli_data(), print = FALSE)
      flush_messages()
    },
    report_end = function(progress = NULL, seconds_elapsed = NULL) {
      message("")
    }
  )
)
