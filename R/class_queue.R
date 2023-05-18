queue_init <- function(
  subclass = c("parallel", "sequential"),
  names = character(0),
  ranks = integer(0)
) {
  subclass <- match.arg(subclass)
  switch(
    subclass,
    parallel = parallel_init(names = names, ranks = ranks),
    sequential = sequential_init(names = names)
  )
}

queue_new <- function(data = NULL) {
  queue_class$new(data)
}

queue_class <- R6::R6Class(
  classname = "tar_queue",
  class = FALSE,
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    data = NULL,
    initialize = function(data = NULL) {
      self$data <- data
    },
    is_nonempty = function() {
      length(self$data) > 0L
    },
    dequeue = function() {
    },
    abridge = function() {
      while (self$is_nonempty()) self$dequeue()
    },
    prepend = function(names, ranks = NULL) {
    },
    append = function(names, ranks = NULL) {
    },
    append0 = function(name) {
    },
    increment_ranks = function(names, by) {
    },
    should_dequeue = function() {
    },
    update_ranks = function(target, scheduler) {
    },
    engraph_branches = function(target, pipeline, scheduler) {
    },
    branch_ranks = function(children, scheduler) {
      integer(0)
    }
  )
)
