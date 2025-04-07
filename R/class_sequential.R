sequential_init <- function(names = character(0), step = 1e5L) {
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
      self$head <- as.integer(self$tail > 0L)
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
      if (.subset2(self, "is_nonempty")()) {
        head <- .subset2(self, "head")
        tail <- .subset2(self, "tail")
        self$data <- .subset2(self, "data")[seq(from = head, to = tail)]
        self$tail <- tail - head
        self$head <- 1L
      } else {
        self$data <- character(0L)
        self$head <- 0L
        self$tail <- 0L
      }
    },
    extend = function() {
      .subset2(self, "clean")()
      self$data <- c(.subset2(self, "data"), rep(NA_character_, self$step))
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
        .subset2(self, "extend")()
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
      self$data <- c(.subset2(self, "data"), names)
      self$tail <- .subset2(self, "tail") + length(names)
    },
    validate = function() {
      tar_assert_chr(self$data)
      tar_assert_int(self$head)
      tar_assert_int(self$tail)
      tar_assert_int(self$step)
      tar_assert_true(self$head <= self$tail)
    }
  )
)