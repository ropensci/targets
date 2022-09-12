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
    report_started = function(target, progress = NULL) {
      cli_start(
        target_get_name(target),
        target_get_type_cli(target),
        time_stamp = TRUE
      )
    },
    report_built = function(target, progress) {
      cli_built(
        target_get_name(target),
        target_get_type_cli(target),
        time_stamp = TRUE,
        seconds_elapsed = target$metrics$seconds
      )
    },
    report_skipped = function(target, progress) {
      cli_skip(
        target_get_name(target),
        target_get_type_cli(target),
        time_stamp = TRUE
      )
    },
    report_errored = function(target, progress = NULL) {
      cli_error(
        target_get_name(target),
        target_get_type_cli(target),
        time_stamp = TRUE
      )
    },
    report_canceled = function(target = NULL, progress = NULL) {
      cli_cancel(
        target_get_name(target),
        target_get_type_cli(target),
        time_stamp = TRUE
      )
    },
    report_workspace = function(target) {
      cli_workspace(target_get_name(target), time_stamp = TRUE)
    },
    report_end = function(progress = NULL, seconds_elapsed = NULL) {
      progress$cli_end(time_stamp = TRUE, seconds_elapsed = seconds_elapsed)
      super$report_end(progress)
    }
  )
)
