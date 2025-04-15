# The sequential queue is a queue structure that attempts to achieve
# approximately O(1) complexity for common methods.
# clean(), extend(), and prepend() are probably around O(n)
# and do not need to be called often.
#
# The `portable = FALSE` setting in R6Class() is super important because
# it allows elements to be directly referenced, and the `<<-`
# operator can be used for assignment. In portable classes,
# either `self$` or `private$` must be used explicitly,
# which causes entire long vectors to be deep-copied in cases
# where the code just needs to replace a single vector element.
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
      tail <<- length(data)
      head <<- 1L
      step <<- as.integer(step)
    },
    is_nonempty = function() {
      tail > 0L && head <= tail
    },
    should_dequeue = function() {
      tail > 0L && head <= tail
    },
    clean = function() {
      if (head > 1L) {
        data <<- data[-seq(head - 1L)]
        tail <<- tail - head + 1L
        head <<- 1L
      }
    },
    extend = function(n) {
      clean()
      data <<- c(data, rep(NA_character_, max(n, step)))
    },
    dequeue = function() {
      if (is_nonempty()) {
        out <- .subset(data, head)
        head <<- head + 1L
        return(out)
      } else {
        return(NULL)
      }
    },
    append = function(names, ranks = NULL) {
      n <- length(names)
      if (length(data) - tail < n) {
        extend(n)
      }
      data[seq_len(n) + tail] <<- names
      tail <<- tail + n
    },
    prepend = function(names, ranks = NULL) {
      clean()
      data <<- c(names, data)
      tail <<- tail + length(names)
    },
    reset = function() {
      data <<- character(0L)
      head <<- 1L
      tail <<- 0L
    },
    validate = function() {
      tar_assert_chr(data)
      tar_assert_int(head)
      tar_assert_int(tail)
      tar_assert_int(step)
    }
  )
)
