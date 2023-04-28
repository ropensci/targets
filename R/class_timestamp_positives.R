timestamp_positives_new <- function(seconds_interval = NULL) {
  timestamp_positives_class$new(seconds_interval = seconds_interval)
}

timestamp_positives_class <- R6::R6Class(
  classname = "tar_timestamp_positives",
  inherit = timestamp_class,
  class = FALSE,
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    report_skipped = function(target, progress) {
    }
  )
)
