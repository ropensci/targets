verbose_new <- function(seconds_interval = NULL) {
  verbose_class$new(seconds_interval = seconds_interval)
}

verbose_class <- R6::R6Class(
  classname = "tar_verbose",
  inherit = reporter_class,
  class = FALSE,
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    flush_messages = function() {
      if (!is.null(self$buffer)) {
        message(paste(self$buffer, collapse = "\n"))
        self$buffer <- NULL
      }
    },
    buffer_message = function(msg) {
      self$buffer[length(self$buffer) + 1L] <- msg
      self$poll()
    },
    report_dispatched = function(target, progress = NULL, pending = FALSE) {
      self$buffer_message(
        cli_dispatched(
          target_get_name(target),
          target_get_type_cli(target),
          print = FALSE,
          pending = pending
        )
      )
    },
    report_completed = function(target, progress = NULL) {
      self$buffer_message(
        cli_completed(
          name = target_get_name(target),
          prefix = target_get_type_cli(target),
          seconds_elapsed = target$metrics$seconds,
          bytes_storage = target$file$bytes,
          print = FALSE
        )
      )
    },
    report_skipped = function(target, progress = NULL) {
      now <- time_seconds_local()
      skipped <- .subset2(.subset2(progress, "skipped"), "count")
      if ((now - seconds_skipped) > reporter_seconds_skipped) {
        self$buffer_message(cli_skip_many(skipped = skipped, print = FALSE))
        self$seconds_skipped <- now
      }
    },
    report_errored = function(target, progress = NULL) {
      self$buffer_message(
        cli_error(
          target_get_name(target),
          target_get_type_cli(target),
          print = FALSE
        )
      )
    },
    report_canceled = function(target = NULL, progress = NULL) {
      self$buffer_message(
        cli_cancel(
          target_get_name(target),
          target_get_type_cli(target),
          print = FALSE
        )
      )
    },
    report_workspace = function(target) {
      self$buffer_message(cli_workspace(target_get_name(target), print = FALSE))
    },
    report_retry = function(target, progress = NULL) {
      self$buffer_message(
        cli_retry(
          target_get_name(target),
          target_get_type_cli(target),
          print = FALSE
        )
      )
    },
    report_finalize = function(progress = NULL) {
      self$flush_messages()
    },
    report_end = function(progress = NULL, seconds_elapsed = NULL) {
      progress$cli_end(seconds_elapsed = seconds_elapsed)
      super$report_end(progress)
    }
  )
)
