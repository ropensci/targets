verbose_new <- function(seconds_interval = 0) {
  verbose_class$new(seconds_interval = seconds_interval)
}

verbose_class <- R6::R6Class(
  classname = "tar_verbose",
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
        paste(
          "dispatched",
          target_get_type_cli(target),
          target_get_name(target)
        )
      )
    },
    report_pattern = function(target) {
      self$buffer_message(paste("defined pattern", target_get_name(target)))
    },
    report_completed = function(target, progress = NULL) {
      self$buffer_message(
        paste(
          "completed",
          target_get_type_cli(target),
          target_get_name(target)
        )
      )
    },
    report_skipped = function(target, progress = NULL) {
      now <- time_seconds_local()
      skipped <- .subset2(.subset2(progress, "skipped"), "count")
      if ((now - seconds_skipped) > reporter_seconds_skipped) {
        self$buffer_message(paste("skipped", skipped, "targets"))
        self$seconds_skipped <- now
      }
    },
    report_errored = function(target, progress = NULL) {
      self$buffer_message(
        paste(
          "errored",
          target_get_type_cli(target),
          target_get_name(target)
        )
      )
    },
    report_canceled = function(target = NULL, progress = NULL) {
      self$buffer_message(
        paste(
          "canceled",
          target_get_type_cli(target),
          target_get_name(target)
        )
      )
    },
    report_workspace = function(target) {
      self$buffer_message(
        paste(
          "record workspace",
          target_get_name(target)
        )
      )
    },
    report_workspace_upload = function(target) {
      self$buffer_message(
        paste(
          "upload workspace",
          target_get_name(target)
        )
      )
    },
    report_retry = function(target, progress = NULL) {
      self$buffer_message(
        paste(
          "retry",
          target_get_type_cli(target),
          target_get_name(target)
        )
      )
    },
    report_finalize = function(progress = NULL) {
      self$flush_messages()
    },
    report_end = function(progress = NULL, seconds_elapsed = NULL) {
      progress$cli_end(seconds_elapsed = seconds_elapsed)
      super$report_end(progress)
    }
  )
)
