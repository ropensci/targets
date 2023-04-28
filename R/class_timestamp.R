timestamp_new <- function(seconds_interval = NULL) {
  timestamp_class$new(seconds_interval = seconds_interval)
}

timestamp_class <- R6::R6Class(
  classname = "tar_timestamp",
  inherit = reporter_class,
  class = FALSE,
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    dequeue = function() {
      if (!is.null(self$queue)) {
        message(paste(self$queue, collapse = "\n"))
        self$queue <- NULL
      }
    },
    enqueue = function(msg) {
      self$queue[length(self$queue) + 1L] <- msg
      self$poll()
    },
    report_started = function(target, progress = NULL) {
      self$enqueue(
        cli_start(
          target_get_name(target),
          target_get_type_cli(target),
          time_stamp = TRUE,
          print = FALSE
        )
      )
    },
    report_built = function(target, progress) {
      self$enqueue(
        cli_built(
          target_get_name(target),
          target_get_type_cli(target),
          time_stamp = TRUE,
          seconds_elapsed = target$metrics$seconds,
          print = FALSE
        )
      )
    },
    report_skipped = function(target, progress) {
      self$enqueue(
        cli_skip(
          target_get_name(target),
          target_get_type_cli(target),
          time_stamp = TRUE,
          print = FALSE
        )
      )
    },
    report_errored = function(target, progress = NULL) {
      self$enqueue(
        cli_error(
          target_get_name(target),
          target_get_type_cli(target),
          time_stamp = TRUE,
          print = FALSE
        )
      )
    },
    report_canceled = function(target = NULL, progress = NULL) {
      self$enqueue(
        cli_cancel(
          target_get_name(target),
          target_get_type_cli(target),
          time_stamp = TRUE,
          print = FALSE
        )
      )
    },
    report_workspace = function(target) {
      self$enqueue(
        cli_workspace(
          target_get_name(target),
          time_stamp = TRUE,
          print = FALSE
        )
      )
    },
    report_end = function(progress = NULL, seconds_elapsed = NULL) {
      self$dequeue()
      progress$cli_end(time_stamp = TRUE, seconds_elapsed = seconds_elapsed)
      super$report_end(progress)
    }
  )
)
