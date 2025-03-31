reporter_init <- function(reporter = "verbose", seconds_interval = 0.5) {
  switch(
    reporter,
    forecast = forecast_new(seconds_interval = seconds_interval),
    silent = silent_new(),
    summary = summary_new(seconds_interval = seconds_interval),
    timestamp = timestamp_new(seconds_interval = seconds_interval),
    timestamp_positives = timestamp_positives_new(
      seconds_interval = seconds_interval
    ),
    verbose = verbose_new(seconds_interval = seconds_interval),
    verbose_positives = verbose_positives_new(
      seconds_interval = seconds_interval
    ),
    tar_throw_validate("unsupported reporter")
  )
}

reporter_new <- function(seconds_interval = 0.5) {
  reporter_class$new(seconds_interval = seconds_interval)
}

reporter_class <- R6::R6Class(
  classname = "tar_reporter",
  class = FALSE,
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    seconds_interval = 0,
    buffer = NULL,
    seconds_flushed = -Inf,
    seconds_skipped = -Inf,
    initialize = function(seconds_interval = NULL) {
      self$seconds_interval <- seconds_interval
    },
    poll = function() {
      now <- time_seconds_local()
      flush <- (now - .subset2(self, "seconds_flushed")) >
        .subset2(self, "seconds_interval")
      if (flush) {
        .subset2(self, "flush_messages")()
        self$seconds_flushed <- now
      }
    },
    report_start = function() {
    },
    report_error = function(error) {
      tar_message_run("Error: ", error, "\n")
    },
    report_end = function(progress = NULL, seconds_elapsed = NULL) {
      if (any(progress$errored$count > 1L)) {
        cli_errored(progress$errored$count)
      }
      if (any(progress$warned$count > 0L)) {
        cli_warned(progress$warned$count)
      }
      cli_reset()
    },
    report_dispatched = function(
      target = NULL,
      progress = NULL,
      pending = FALSE
    ) {
    },
    report_pattern = function(target = NULL) {
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
    report_finalize = function(progress = NULL) {
    },
    flush_messages = function() {
    },
    validate = function() {
    }
  )
)

reporter_seconds_skipped <- 1
