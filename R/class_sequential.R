sequential_init <- function(names = character(0)) {
  sequential_new(names)
}

sequential_new <- function(data = NULL) {
  sequential_class$new(data)
}

sequential_class <- R6::R6Class(
  classname = "tar_sequential",
  inherit = queue_class,
  class = FALSE,
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    dequeue = function() {
      head <- self$data[1L]
      self$data <- self$data[-1L]
      head
    },
    prepend = function(names, ranks = NULL) {
      self$data <- c(names, self$data)
    },
    append = function(names, ranks = NULL) {
      self$data <- c(self$data, names)
    },
    append0 = function(name) {
      self$data[length(self$data) + 1L] <- name
    },
    should_dequeue = function() {
      self$is_nonempty()
    },
    validate = function() {
      tar_assert_chr(self$data)
    }
  )
)
