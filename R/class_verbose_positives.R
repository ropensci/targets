verbose_positives_new <- function(seconds_interval = NULL) {
  verbose_positives_class$new(seconds_interval = seconds_interval)
}

verbose_positives_class <- R6::R6Class(
  classname = "tar_verbose_positives",
  inherit = verbose_class,
  class = FALSE,
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    report_skipped = function(target, progress = NULL) {
    }
  )
)
