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
    report_running = function(target, progress = NULL) {
      msg <- paste(time_stamp(), "run", target_get_type_cli(target))
      cli_target(target_get_name(target), msg)
    },
    report_skipped = function(target, progress) {
      msg <- paste(time_stamp(), "skip", target_get_type_cli(target))
      cli_skip(target_get_name(target), msg)
    },
    report_errored = function(target, progress = NULL) {
      cli_error(target_get_name(target), time_stamp())
    },
    report_cancelled = function(target = NULL, progress = NULL) {
      cli_cancel(target_get_name(target), time_stamp())
    },
    report_end = function(progress = NULL) {
      if (progress$uptodate()) {
        cli_uptodate()
      }
      if (any(progress$warned$count > 0L)) {
        cli_warned(progress$warned$count)
      }
    }
  )
)
