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
      cli_df_header(progress_init()$cli_data())
    },
    report_started = function(target = NULL, progress) {
      cli_df_body(progress$cli_data())
    },
    report_built = function(target = NULL, progress) {
      cli_df_body(progress$cli_data())
    },
    report_skipped = function(target = NULL, progress) {
      cli_df_body(progress$cli_data())
    },
    report_errored = function(target = NULL, progress) {
      cli_df_body(progress$cli_data())
    },
    report_canceled = function(target = NULL, progress) {
      cli_df_body(progress$cli_data())
    },
    report_end = function(progress) {
      cli_df_body(progress$cli_data())
      message("")
    }
  )
)
