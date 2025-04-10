terse_new <- function() {
  terse_class$new()
}

terse_class <- R6::R6Class(
  classname = "tar_terse",
  inherit = reporter_class,
  class = FALSE,
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    report_pattern = function(target, progress = NULL) {
      cli::cli_text(
        sprintf(
          "%s {.pkg %s} declared [%s branches]",
          cli::col_silver("+"),
          target_get_name(target),
          junction_length(.subset2(target, "junction"))
        )
      )
    },
    report_dispatched = function(target, progress = NULL, pending = FALSE) {
      if (!inherits(target, "tar_branch")) {
        cli::cli_text(
          sprintf(
            "%s {.pkg %s} dispatched",
            cli::col_silver("+"),
            target_get_name(target)
          )
        )
      }
    },
    report_completed = function(target, progress = NULL) {
      if (!inherits(target, "tar_branch")) {
        cli::cli_alert_success(
          sprintf(
            "{.pkg %s} completed %s",
            target_get_name(target),
            cli_resources(target)
          )
        )
      }
    },
    report_errored = function(target, progress = NULL) {
      cli::cli_alert_danger(
        sprintf(
          "{.pkg %s} errored",
          target_get_name(target)
        )
      )
    },
    report_end = function(progress = NULL, seconds_elapsed = NULL) {
      if (!is.null(progress)) {
        progress$cli_end(seconds_elapsed = seconds_elapsed)
      }
      super$report_end(progress)
    }
  )
)
