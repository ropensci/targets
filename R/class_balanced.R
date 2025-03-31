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
        cli::cli_alert(
          "dispatched %s {.pkg %s}",
          target_get_type_cli(target),
          target_get_name(target)
        )
      }
    },
    report_pattern = function(target) {
      cli::cli_alert(
        sprintf("defined pattern {.pkg %s}", target_get_name(target))
      )
    },
    report_completed = function(target, progress = NULL) {
      if (inherits(target, "tar_branch")) {
        self$report_progress(progress)
      } else {
        cli::cli_alert_success(
          sprintf(
            "completed %s {.pkg %s}",
            target_get_type_cli(target),
            target_get_name(target)
          )
        )
      }
    },
    report_errored = function(target, progress = NULL) {
      cli::cli_alert_danger(
        "errored %s {.pkg %s}",
        target_get_type_cli(target),
        target_get_name(target)
      )
    },
    report_outdated = function(outdated) {
      self$report_progress(outdated$scheduler$progress)
    },
    report_end = function(progress = NULL, seconds_elapsed = NULL) {
      if (!is.null(progress)) {
        progress$cli_end(seconds_elapsed = seconds_elapsed)
      }
      cli::cli_progress_done(.envir = .subset2(private, ".bar"))
      super$report_end(progress)
    }
  )
)
