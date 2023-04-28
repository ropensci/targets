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
          print = FALSE
        )
      )
    },
    report_built = function(target, progress = NULL) {
      self$enqueue(
        cli_built(
          name = target_get_name(target),
          prefix = target_get_type_cli(target),
          seconds_elapsed = target$metrics$seconds,
          print = FALSE
        )
      )
    },
    report_skipped = function(target, progress = NULL) {
      self$enqueue(
        cli_skip(
          target_get_name(target),
          target_get_type_cli(target),
          print = FALSE
        )
      )
    },
    report_errored = function(target, progress = NULL) {
      self$enqueue(
        cli_error(
          target_get_name(target),
          target_get_type_cli(target),
          print = FALSE
        )
      )
    },
    report_canceled = function(target = NULL, progress = NULL) {
      self$enqueue(
        cli_cancel(
          target_get_name(target),
          target_get_type_cli(target),
          print = FALSE
        )
      )
    },
    report_workspace = function(target) {
      self$enqueue(cli_workspace(target_get_name(target), print = FALSE))
    },
    report_end = function(progress = NULL, seconds_elapsed = NULL) {
      self$dequeue()
      progress$cli_end(seconds_elapsed = seconds_elapsed)
      super$report_end(progress)
    }
  )
)
