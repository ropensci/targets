verbose_positives_new <- function() {
  verbose_positives_class$new()
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
