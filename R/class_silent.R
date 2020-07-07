silent_new <- function() {
  silent_class$new()
}

silent_class <- R6::R6Class(
  classname = "tar_silent",
  inherit = reporter_class,
  class = FALSE,
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    report_running = function(target = NULL, progress = NULL) {
    },
    report_skipped = function(target, progress) {
    },
    report_errored = function(target = NULL, progress = NULL) {
    },
    report_cancelled = function(target = NULL, progress = NULL) {
    },
    report_outdated = function(checked, outdated) {
    }
  )
)
