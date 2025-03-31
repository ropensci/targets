balanced_new <- function() {
  balanced_class$new()
}

balanced_class <- R6::R6Class(
  classname = "tar_balanced",
  inherit = reporter_class,
  class = FALSE,
  portable = FALSE,
  cloneable = FALSE,
  private = list(
    .bar = NULL
  ),
  public = list(
    bar = NULL,
    initialize = function() {
      private$.bar <- new.env(parent = globalenv())
    },
    report_start = function() {
      cli::cli_progress_bar(
        name = paste("pipeline", Sys.time()),
        format = "{cli::pb_bar} {cli::pb_current}/{cli::pb_total}",
        .envir = .subset2(private, ".bar"),
        auto_terminate = FALSE,
        clear = TRUE
      )
    },
    report_progress = function(progress) {
      cli::cli_progress_update(
        set = progress$completed$count +
          progress$errored$count +
          progress$canceled$count +
          progress$skipped$count,
        total = progress$completed$count +
          progress$errored$count +
          progress$canceled$count +
          progress$dispatched$count +
          progress$queued$count +
          progress$skipped$count,
        .envir = .subset2(private, ".bar")
      )
    },
    report_skipped = function(target = NULL, progress = NULL) {
      self$report_progress(progress)
    },
    report_dispatched = function(target, progress = NULL, pending = FALSE) {
      if (inherits(target, "tar_branch")) {
        self$report_progress(progress)
      } else {
        cli::cli_progress_output(
          cli_dispatched(
            target_get_name(target),
            target_get_type_cli(target),
            print = FALSE,
            pending = pending
          ),
          .envir = .subset2(private, ".bar")
        )
      }
    },
    report_pattern = function(target) {
      cli::cli_progress_output(
        cli_pattern(
          target_get_name(target),
          branches = length(target$junction$index),
          print = FALSE
        ),
        .envir = .subset2(private, ".bar")
      )
    },
    report_completed = function(target, progress = NULL) {
      if (inherits(target, "tar_branch")) {
        self$report_progress(progress)
      } else {
        cli::cli_progress_output(
          cli_completed(
            name = target_get_name(target),
            prefix = target_get_type_cli(target),
            seconds_elapsed = target$metrics$seconds %|||%
              target$patternview$seconds,
            bytes_storage = target$file$bytes %|||%
              target$patternview$bytes,
            print = FALSE
          ),
          .envir = .subset2(private, ".bar")
        )
      }
    },
    report_errored = function(target, progress = NULL) {
      cli::cli_progress_output(
        cli_error(
          target_get_name(target),
          target_get_type_cli(target),
          print = FALSE
        ),
        .envir = .subset2(private, ".bar")
      )
    },
    report_end = function(progress = NULL, seconds_elapsed = NULL) {
      cli::cli_progress_output(
        progress$cli_end(seconds_elapsed = seconds_elapsed, print = FALSE),
        .envir = .subset2(private, ".bar")
      )
      cli::cli_progress_done(.envir = .subset2(private, ".bar"))
      super$report_end(progress)
    }
  )
)
