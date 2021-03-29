forecast_new <- function() {
  forecast_class$new()
}

forecast_class <- R6::R6Class(
  classname = "tar_forecast",
  inherit = reporter_class,
  class = FALSE,
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    report_start = function() {
      cli_df_header(outdated_init()$cli_data())
    },
    report_skipped = function(target, progress) {
    },
    report_outdated = function(outdated) {
      cli_df_body(outdated$cli_data())
    },
    report_end = function(progress = NULL) {
      message("")
    }
  )
)
