reporter_init <- function(reporter = "balanced") {
  switch(
    reporter,
    balanced = balanced_new(),
    terse = terse_new(),
    silent = silent_new(),
    timestamp = timestamp_new(),
    verbose = verbose_new(),
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
    seconds_skipped = -Inf,
    report_start = function() {
    },
    report_error = function(error) {
      tar_message_run("Error: ", error, "\n")
    },
    report_end = function(progress = NULL, seconds_elapsed = NULL) {
      if (any(progress$errored$count > 1L)) {
        tar_warn_run(
          progress$errored$count,
          " targets produced errors. ",
          "Run targets::tar_meta(fields = error, complete_only = TRUE) ",
          "for the messages."
        )
      }
      if (any(progress$warned$count > 0L)) {
        tar_warn_run(
          progress$warned$count,
          " targets produced warnings. ",
          "Run targets::tar_meta(fields = warnings, complete_only = TRUE) ",
          "for the messages."
        )
      }
      cli_reset()
    },
    report_dispatched = function(
      target = NULL,
      progress = NULL,
      pending = FALSE
    ) {
    },
    report_pattern = function(target = NULL, progress = NULL) {
    },
    report_completed = function(target = NULL, progress = NULL) {
    },
    report_skipped = function(target = NULL, progress = NULL) {
    },
    report_errored = function(target = NULL, progress = NULL) {
    },
    report_canceled = function(target = NULL, progress = NULL) {
    },
    report_outdated = function(outdated) {
    },
    report_outdated_end = function(outdated) {
    },
    report_workspace = function(target) {
    },
    report_workspace_upload = function(target) {
    },
    report_retry = function(target = NULL, progress = NULL) {
    },
    validate = function() {
    }
  )
)

reporter_seconds_skipped <- 1
