summary_new <- function() {
  summary_class$new()
}

summary_class <- R6::R6Class(
  classname = "tar_summary",
  inherit = reporter_class,
  class = FALSE,
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    time = NULL,
    report_start = function() {
      self$time <- proc.time()["elapsed"]
      cli_df_header(progress_init(path = tempfile())$cli_data())
    },
    report_progress = function(progress) {
      cli_df_body(progress$cli_data())
    },
    report_error = function(error) {
    },
    report_started = function(target = NULL, progress) {
      self$report_progress(progress)
    },
    report_built = function(target = NULL, progress) {
      self$report_progress(progress)
    },
    report_skipped = function(target = NULL, progress) {
      time <- proc.time()["elapsed"]
      # nocov start
      # Covered in tests/interactive/test-reporter.R.
      if (time - self$time > 0.25) {
        self$report_progress(progress)
        self$time <- time
      }
      # nocov end
    },
    report_errored = function(target = NULL, progress) {
      self$report_progress(progress)
    },
    report_canceled = function(target = NULL, progress) {
      self$report_progress(progress)
    },
    report_end = function(progress, seconds_elapsed = NULL) {
      self$report_progress(progress)
      message("")
    }
  )
)
