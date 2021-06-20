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
    time = NULL,
    report_start = function() {
      self$time <- proc.time()["elapsed"]
    },
    report_skipped = function(target, progress) {
    },
    report_outdated = function(outdated) {
      time <- proc.time()["elapsed"]
      # nocov start
      # Covered in tests/interactive/test-reporter.R.
      if (time - self$time > 0.25) {
        cli_df_body_oneline(outdated$cli_data())
        self$time <- time
      }
      # nocov end
    },
    report_end = function(progress = NULL) {
      msg <- paste(c("\r", rep(" ", getOption("width") - 1L)), collapse = "")
      message(msg)
    }
  )
)
