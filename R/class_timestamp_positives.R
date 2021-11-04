timestamp_positives_new <- function() {
  timestamp_positives_class$new()
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
