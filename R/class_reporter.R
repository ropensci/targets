reporter_init <- function(reporter = "verbose") {
  switch(
    reporter,
    forecast = forecast_new(),
    silent = silent_new(),
    summary = summary_new(),
    timestamp = timestamp_new(),
    timestamp_positives = timestamp_positives_new(),
    verbose = verbose_new(),
    verbose_positives = verbose_positives_new(),
    tar_throw_validate("unsupported reporter")
  )
}

reporter_new <- function() {
  reporter_class$new()
}

reporter_class <- R6::R6Class(
  classname = "tar_reporter",
  class = FALSE,
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    report_start = function() {
    },
    report_error = function(error) {
      tar_message_run("Error: ", error, "\n")
    },
    report_end = function(progress = NULL) {
      if (any(progress$errored$count > 1L)) {
        cli_errored(progress$errored$count)
      }
      if (any(progress$warned$count > 0L)) {
        cli_warned(progress$warned$count)
      }
    },
    report_started = function(target = NULL, progress = NULL) {
    },
    report_built = function(target = NULL, progress = NULL) {
    },
    report_skipped = function(target = NULL, progress = NULL) {
    },
    report_errored = function(target = NULL, progress = NULL) {
    },
    report_canceled = function(target = NULL, progress = NULL) {
    },
    report_outdated = function(outdated) {
    },
    report_workspace = function(target) {
    },
    validate = function() {
    }
  )
)
