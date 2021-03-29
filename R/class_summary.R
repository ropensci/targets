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
      progress_init()$cli_header()
    },
    report_started = function(target, progress) {
      progress$cli_update()
    },
    report_skipped = function(target, progress) {
      progress$cli_update()
    },
    report_errored = function(target, progress) {
      progress$cli_update()
    },
    report_canceled = function(target = NULL, progress = NULL) {
      progress$cli_update()
    },
    report_end = function(progress) {
      progress$cli_update()
      message("")
    }
  )
)
