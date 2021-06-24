visual_new <- function(
  network = NULL
) {
  visual_class$new(
    network = network
  )
}

visual_class <- R6::R6Class(
  classname = "tar_visual",
  class = FALSE,
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    network = NULL,
    initialize = function(
      network = NULL
    ) {
      self$network <- network
    },
    update_network = function() {
      self$network$update()
    },
    validate = function() {
      self$network$validate()
      invisible()
    }
  )
)
