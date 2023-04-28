summary_new <- function(seconds_interval = NULL) {
  summary_class$new(seconds_interval = seconds_interval)
}

summary_class <- R6::R6Class(
  classname = "tar_summary",
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
    report_start = function() {
      cli_df_header(progress_init(path = tempfile())$cli_data())
    },
    report_progress = function(progress, force = FALSE) {
      self$queue <- cli_df_body(progress$cli_data(), print = FALSE)
      self$poll()
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
      self$report_progress(progress)
    },
    report_errored = function(target = NULL, progress) {
      self$report_progress(progress)
    },
    report_canceled = function(target = NULL, progress) {
      self$report_progress(progress)
    },
    report_end = function(progress, seconds_elapsed = NULL) {
      self$report_progress(progress)
      self$dequeue()
      message("")
    }
  )
)
