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
    .bar_envir = NULL,
    .bar_id = NULL
  ),
  public = list(
    report_start = function() {
      private$.bar_envir <- new.env(parent = globalenv())
      private$.bar_id <- cli::cli_progress_bar(
        format = sprintf(
          paste(
            cli::symbol$arrow_right,
            "{cli::pb_total} targets {cli::pb_bar}",
            "[{cli::pb_elapsed}, {completed}+, {skipped}-]"
          )
        ),
        .envir = .subset2(private, ".bar_envir"),
        auto_terminate = FALSE,
        clear = TRUE
      )
    },
    report_progress = function(progress, force = FALSE) {
      envir <- .subset2(private, ".bar_envir")
      envir$completed <- progress$completed$count # For the cli format string.
      envir$skipped <- progress$skipped$count # For the cli format string.
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
        force = force,
        .envir = envir,
        id = .subset2(private, ".bar_id")
      )
    },
    report_pattern = function(target, progress = NULL) {
      cli::cli_progress_output(
        sprintf(
          "%s {.pkg %s} declared [%s branches]",
          cli::col_silver("+"),
          target_get_name(target),
          junction_length(.subset2(target, "junction"))
        ),
        .envir = .subset2(private, ".bar_envir"),
        id = .subset2(private, ".bar_id")
      )
      self$report_progress(progress, force = TRUE)
    },
    report_skipped = function(target = NULL, progress = NULL) {
      self$report_progress(progress)
    },
    report_dispatched = function(target, progress = NULL, pending = FALSE) {
      self$report_progress(progress)
      if (!inherits(target, "tar_branch")) {
        cli::cli_progress_output(
          sprintf(
            "%s {.pkg %s} dispatched",
            cli::col_silver("+"),
            target_get_name(target)
          ),
          .envir = .subset2(private, ".bar_envir"),
          id = .subset2(private, ".bar_id")
        )
      }
    },
    report_completed = function(target, progress = NULL) {
      self$report_progress(progress)
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
      self$report_progress(progress)
      cli::cli_alert_danger(
        sprintf(
          "{.pkg %s} errored",
          target_get_name(target)
        )
      )
    },
    report_outdated = function(outdated) {
      self$report_progress(outdated$scheduler$progress)
    },
    report_end = function(progress = NULL, seconds_elapsed = NULL) {
      cli::cli_progress_done(
        .envir = .subset2(private, ".bar_envir"),
        id = .subset2(private, ".bar_id")
      )
      # ansi_show_cursor() is part of cli_progress_cleanup(),
      # but we don't want all of cli_progress_cleanup() because it terminates
      # all progress bars.
      cli::ansi_show_cursor()
      if (!is.null(progress)) {
        progress$cli_end(seconds_elapsed = seconds_elapsed)
      }
      super$report_end(progress)
    }
  )
)
