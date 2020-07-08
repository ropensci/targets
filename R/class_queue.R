queue_init <- function(names = character(0), ranks = integer(0)) {
  data <- ranks
  names(data) <- as.character(names)
  queue_new(data, counter_init(names))
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
    get_names = function() {
      names(self$data)
    },
    get_ranks = function() {
      unname(self$data)
    },
    exists_name = function(name) {
      counter_exists_name(self$counter, name)
    },
    filter_exists = function(names) {
      counter_filter_exists(self$counter, names)
    },
    dequeue = function() {
      index <- which.min(self$data)
      head <- names(self$data[index])
      self$data <- self$data[-index]
      counter <- self$counter
      counter_del_names(counter, head)
      head
    },
    enqueue = function(names, ranks = NULL) {
      new_ranks <- ranks %||% rep(0L, length(names))
      ranks <- c(self$get_ranks(), new_ranks)
      names <- c(self$get_names(), names)
      names(ranks) <- names
      self$data <- ranks
      counter <- self$counter
      counter_set_names(counter, names)
      invisible()
    },
    increment_ranks = function(names, by) {
      self$data[names] <- self$data[names] + by
    },
    set_ranks = function(names, ranks) {
      self$data[names] <- ranks
    },
    get_count = function() {
      length(self$data)
    },
    is_nonempty = function() {
      self$get_count() > 0L
    },
    should_dequeue = function() {
      any(as.integer(ceiling(self$get_ranks())) == 0L)
    },
    validate_names = function(names) {
      assert_chr(names)
      if (anyNA(names) || anyDuplicated(names)) {
        throw_validate("names must unique finite character strings.")
      }
    },
    validate_ranks = function(ranks) {
      if (anyNA(ranks) || any(ranks <= -1L)) {
        throw_validate("ranks must be nonmissing numerics greater than -1.")
      }
    },
    validate = function() {
      self$validate_names(self$get_names())
      self$validate_ranks(self$get_ranks())
      counter_validate(self$counter)
    }
  )
)
