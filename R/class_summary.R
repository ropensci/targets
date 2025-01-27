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
    flush_messages = function() {
      if (!is.null(self$buffer)) {
        message(self$buffer, appendLF = FALSE)
        self$buffer <- NULL
      }
    },
    report_start = function() {
      cli_df_header(progress_init(path_store = tempfile())$cli_data())
    },
    report_progress = function(progress, force = FALSE) {
      self$buffer <- cli_df_body(progress$cli_data(), print = FALSE)
      self$poll()
    },
    report_error = function(error) {
    },
    report_dispatched = function(target = NULL, progress, pending = FALSE) {
      self$report_progress(progress)
    },
    report_completed = function(target = NULL, progress) {
      self$report_progress(progress)
    },
    report_skipped = function(target = NULL, progress) {
      now <- time_seconds_local()
      if ((now - seconds_skipped) > reporter_seconds_skipped) {
        report_progress(progress)
        self$seconds_skipped <- now
      }
    },
    report_errored = function(target = NULL, progress) {
      self$report_progress(progress)
    },
    report_canceled = function(target = NULL, progress) {
      self$report_progress(progress)
    },
    report_retry = function(target = NULL, progress) {
      self$report_progress(progress)
    },
    report_finalize = function(progress) {
      self$report_progress(progress)
      self$flush_messages()
    },
    report_end = function(progress, seconds_elapsed = NULL) {
      message("")
    }
  )
)
