timestamp_new <- function(seconds_interval = NULL) {
  timestamp_class$new(seconds_interval = seconds_interval)
}

timestamp_class <- R6::R6Class(
  classname = "tar_timestamp",
  inherit = reporter_class,
  class = FALSE,
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    flush_messages = function() {
      if (!is.null(self$buffer)) {
        message(paste(self$buffer, collapse = "\n"))
        self$buffer <- NULL
      }
    },
    buffer_message = function(msg) {
      self$buffer[length(self$buffer) + 1L] <- msg
      self$poll()
    },
    report_dispatched = function(target, progress = NULL, pending = FALSE) {
      self$buffer_message(
        cli_dispatched(
          target_get_name(target),
          target_get_type_cli(target),
          time_stamp = TRUE,
          print = FALSE,
          pending = pending
        )
      )
    },
    report_completed = function(target, progress) {
      self$buffer_message(
        cli_completed(
          target_get_name(target),
          target_get_type_cli(target),
          time_stamp = TRUE,
          seconds_elapsed = target$metrics$seconds,
          print = FALSE
        )
      )
    },
    report_skipped = function(target, progress) {
      self$buffer_message(
        cli_skip(
          target_get_name(target),
          target_get_type_cli(target),
          time_stamp = TRUE,
          print = FALSE
        )
      )
    },
    report_errored = function(target, progress = NULL) {
      self$buffer_message(
        cli_error(
          target_get_name(target),
          target_get_type_cli(target),
          time_stamp = TRUE,
          print = FALSE
        )
      )
    },
    report_canceled = function(target = NULL, progress = NULL) {
      self$buffer_message(
        cli_cancel(
          target_get_name(target),
          target_get_type_cli(target),
          time_stamp = TRUE,
          print = FALSE
        )
      )
    },
    report_workspace = function(target) {
      self$buffer_message(
        cli_workspace(
          target_get_name(target),
          time_stamp = TRUE,
          print = FALSE
        )
      )
    },
    report_end = function(progress = NULL, seconds_elapsed = NULL) {
      self$flush_messages()
      progress$cli_end(time_stamp = TRUE, seconds_elapsed = seconds_elapsed)
      super$report_end(progress)
    }
  )
)
