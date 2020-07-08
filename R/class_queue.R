queue_init <- function(
  subclass = c("parallel", "serial"),
  names = character(0),
  ranks = integer(0)
) {
  subclass <- match.arg(subclass)
  switch(
    subclass,
    parallel = parallel_init(names = names, ranks = ranks),
    serial = serial_init(names = names, ranks = ranks),
    throw_validate("unsupported queue")
  )
}

queue_new <- function(data = NULL, counter = NULL) {
  queue_class$new(data, counter)
}

queue_class <- R6::R6Class(
  classname = "tar_queue",
  class = FALSE,
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    data = NULL,
    counter = NULL,
    initialize = function(
      data = NULL,
      counter = NULL
    ) {
      self$data <- data
      self$counter <- counter
    },
    is_nonempty = function() {
      length(self$data) > 0L
    },
    dequeue = function() {
    },
    enqueue = function(names, ranks = NULL) {
    },
    increment_ranks = function(names, by) {
    },
    should_dequeue = function() {
    }
  )
)
