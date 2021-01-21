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
    report_error = function(error) {
    },
    report_start = function() {
      cli_header_progress()
    },
    report_running = function(target, progress) {
      progress$update_cli()
    },
    report_skipped = function(target, progress) {
      progress$update_cli()
    },
    report_errored = function(target, progress) {
      progress$update_cli()
    },
    report_canceled = function(target = NULL, progress = NULL) {
      progress$update_cli()
    },
    report_end = function(progress) {
      progress$update_cli()
    }
  )
)
