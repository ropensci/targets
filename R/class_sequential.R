# The sequential queue is a queue structure that attempts to achieve
# approximately O(1) complexity for common methods.
# clean(), extend(), and prepend() are probably around O(n)
# and do not need to be called often.
sequential_init <- function(names = character(0), step = 1e3L) {
  sequential_new(data = names, step = step)
}

sequential_new <- function(data = NULL, step = NULL) {
  sequential_class$new(data = data, step = step)
}

sequential_class <- R6::R6Class(
  classname = "tar_sequential",
  inherit = queue_class,
  class = FALSE,
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    head = NULL,
    tail = NULL,
    step = NULL,
    initialize = function(data = NULL, step = NULL) {
      super$initialize(data = data)
      self$tail <- length(data)
      self$head <- 1L
      self$step <- as.integer(step)
    },
    is_nonempty = function() {
      head <- .subset2(self, "head")
      tail <- .subset2(self, "tail")
      tail > 0L && head <= tail
    },
    should_dequeue = function() {
      head <- .subset2(self, "head")
      tail <- .subset2(self, "tail")
      tail > 0L && head <= tail
    },
    clean = function() {
      head <- .subset2(self, "head")
      if (head > 1L) {
        self$data <- .subset2(self, "data")[-seq(head - 1L)]
        self$tail <- tail - head + 1L
        self$head <- 1L
      }
    },
    extend = function(n) {
      .subset2(self, "clean")()
      n <- max(n, .subset2(self, "step"))
      self$data <- c(.subset2(self, "data"), rep(NA_character_, n))
    },
    dequeue = function() {
      if (.subset2(self, "is_nonempty")()) {
        head <- .subset2(self, "head")
        out <- .subset(.subset2(self, "data"), head)
        self$head <- head + 1L
        return(out)
      } else {
        return(NULL)
      }
    },
    append = function(names, ranks = NULL) {
      data <- .subset2(self, "data")
      tail <- .subset2(self, "tail")
      if (length(data) - tail < length(names)) {
        .subset2(self, "extend")(length(names))
      }
      index <- 1L
      n <- length(names)
      data <- .subset2(self, "data")
      tail <- .subset2(self, "tail")
      while (index <= n) {
        self$data[index + tail] <- .subset(names, index)
        index <- index + 1L
      }
      self$tail <- tail + n
    },
    prepend = function(names, ranks = NULL) {
      .subset2(self, "clean")()
      self$data <- c(names, .subset2(self, "data"))
      self$tail <- .subset2(self, "tail") + length(names)
    },
    reset = function() {
      self$data <- character(0L)
      self$head <- 1L
      self$tail <- 0L
    },
    validate = function() {
      tar_assert_chr(self$data)
      tar_assert_int(self$head)
      tar_assert_int(self$tail)
      tar_assert_int(self$step)
    }
  )
)
