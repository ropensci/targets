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
    dequeue = function() {
      if (!is.null(self$queue)) {
        message(self$queue, appendLF = FALSE)
        self$queue <- NULL
      }
    },
    report_outdated = function(outdated) {
      self$queue <- cli_df_body_oneline(outdated$cli_data(), print = FALSE)
      self$poll()
    },
    report_end = function(progress = NULL, seconds_elapsed = NULL) {
      self$dequeue()
      msg <- paste(c("\r", rep(" ", getOption("width") - 1L)), collapse = "")
      message(msg)
    }
  )
)
