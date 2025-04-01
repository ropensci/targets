timestamp_new <- function() {
  timestamp_class$new()
}

timestamp_class <- R6::R6Class(
  classname = "tar_timestamp",
  inherit = reporter_class,
  class = FALSE,
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    report_dispatched = function(target, progress = NULL, pending = FALSE) {
      message(
        paste(
          time_stamp_cli(),
          "dispatched",
          target_get_type_cli(target),
          target_get_name(target)
        )
      )
    },
    report_pattern = function(target, progress = NULL) {
      message(
        paste(
          time_stamp_cli(),
          "declared pattern",
          target_get_name(target)
        )
      )
    },
    report_completed = function(target, progress) {
      message(
        paste(
          time_stamp_cli(),
          "completed",
          target_get_type_cli(target),
          target_get_name(target),
          cli_resources(target)
        )
      )
    },
    report_skipped = function(target, progress) {
      now <- time_seconds_local()
      skipped <- .subset2(.subset2(progress, "skipped"), "count")
      if ((now - seconds_skipped) > reporter_seconds_skipped) {
        message(
          paste(time_stamp_cli(), "skipped", skipped, "targets")
        )
        self$seconds_skipped <- now
      }
    },
    report_errored = function(target, progress = NULL) {
      message(
        paste(
          time_stamp_cli(),
          "errored",
          target_get_type_cli(target),
          target_get_name(target)
        )
      )
    },
    report_canceled = function(target = NULL, progress = NULL) {
      message(
        paste(
          time_stamp_cli(),
          "canceled",
          target_get_type_cli(target),
          target_get_name(target)
        )
      )
    },
    report_workspace = function(target) {
      message(
        paste(
          time_stamp_cli(),
          "record workspace",
          target_get_name(target)
        )
      )
    },
    report_workspace_upload = function(target) {
      message(
        paste(
          time_stamp_cli(),
          "upload workspace",
          target_get_name(target)
        )
      )
    },
    report_retry = function(target, progress = NULL) {
      message(
        paste(
          time_stamp_cli(),
          "retry",
          target_get_type_cli(target),
          target_get_name(target)
        )
      )
    },
    report_end = function(progress = NULL, seconds_elapsed = NULL) {
      progress$cli_end(time_stamp = TRUE, seconds_elapsed = seconds_elapsed)
      super$report_end(progress)
    }
  )
)
