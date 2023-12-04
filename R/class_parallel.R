parallel_init <- function(names = character(0), ranks = integer(0)) {
  data <- ranks
  names(data) <- as.character(names)
  parallel_new(data, counter_init(names))
}

parallel_new <- function(data = NULL, counter = NULL) {
  parallel_class$new(data, counter)
}

parallel_class <- R6::R6Class(
  classname = "tar_parallel",
  inherit = queue_class,
  class = FALSE,
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    counter = NULL,
    initialize = function(data = NULL, counter = NULL) {
      super$initialize(data)
      self$counter <- counter
    },
    get_names = function() {
      names(self$data)
    },
    get_ranks = function() {
      unname(self$data)
    },
    dequeue = function() {
      index <- which.min(self$data)
      head <- names(self$data[index])
      self$data <- self$data[-index]
      counter <- self$counter
      counter_del_names(counter, head)
      head
    },
    prepend = function(names, ranks = NULL) {
      new_ranks <- ranks %|||% rep(0L, length(names))
      ranks <- c(new_ranks, self$get_ranks())
      names <- c(names, self$get_names())
      names(ranks) <- names
      self$data <- ranks
      counter <- self$counter
      counter_set_names(counter, names)
      invisible()
    },
    append = function(names, ranks = NULL) {
      new_ranks <- ranks %|||% rep(0L, length(names))
      ranks <- c(self$get_ranks(), new_ranks)
      names <- c(self$get_names(), names)
      names(ranks) <- names
      self$data <- ranks
      counter <- self$counter
      counter_set_names(counter, names)
      invisible()
    },
    increment_ranks = function(names, by) {
      index <- names(self$data) %in% names
      self$data[index] <- self$data[index] + by
    },
    should_dequeue = function() {
      any(as.integer(ceiling(self$get_ranks())) == 0L)
    },
    update_ranks = function(target, scheduler) {
      names <- target_downstream_names(target, scheduler)
      target_decrement_ranks(names, scheduler)
    },
    engraph_branches = function(target, pipeline, scheduler) {
      graph <- scheduler$graph
      graph$insert_edges(junction_upstream_edges(target$junction))
      edges <- data_frame(
        from = target_get_children(target),
        to = target_get_name(target)
      )
      graph$insert_edges(edges)
    },
    branch_ranks = function(children, scheduler) {
      unlist(lapply(children, scheduler$count_unfinished_deps))
    },
    validate_names = function(names) {
      tar_assert_chr(names)
      if (anyNA(names) || anyDuplicated(names)) {
        tar_throw_validate("names must unique finite character strings.")
      }
    },
    validate_ranks = function(ranks) {
      if (anyNA(ranks) || any(ranks <= -1L)) {
        tar_throw_validate("ranks must be nonmissing numerics greater than -1.")
      }
    },
    validate = function() {
      self$validate_names(self$get_names())
      self$validate_ranks(self$get_ranks())
      counter_validate(self$counter)
    }
  )
)
