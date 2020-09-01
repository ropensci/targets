silent_new <- function() {
  silent_class$new()
}

silent_class <- R6::R6Class(
  classname = "tar_silent",
  inherit = reporter_class,
  class = FALSE,
  portable = FALSE,
  cloneable = FALSE,
  public = list()
)
