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
    head = NULL,
    initialize = function(data) {
      super$initialize(data = data)
      self$head <- 1L
    },
    is_nonempty = function() {
      head <= length(data)
    },
    dequeue = function() {
      index <- head
      if (index <= length(data)) {
        self$head <- index + 1L
      }
      .subset(data, index)
    },
    prepend = function(names, ranks = NULL) {
      out <- c(data[seq_len(head - 1L)], names)
      if (head <= length(data)) {
        out <- c(out, data[seq(from = head, to = length(data), by = 1L)])
      }
      self$data <- out
    },
    append = function(names, ranks = NULL) {
      self$data <- c(self$data, names)
    },
    should_dequeue = function() {
      self$is_nonempty()
    },
    validate = function() {
      tar_assert_chr(self$data)
    }
  )
)
