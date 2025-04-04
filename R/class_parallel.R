parallel_init <- function(names = character(0), ranks = integer(0)) {
  data <- ranks
  names(data) <- as.character(names)
  parallel_new(data)
}

parallel_new <- function(data = NULL) {
  parallel_class$new(data)
}

parallel_class <- R6::R6Class(
  classname = "tar_parallel",
  inherit = queue_class,
  class = FALSE,
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    is_nonempty = function() {
      length(self$data) > 0L
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
      head
    },
    prepend = function(names, ranks = NULL) {
      new_ranks <- ranks %|||% rep(0L, length(names))
      ranks <- c(new_ranks, self$get_ranks())
      names <- c(names, self$get_names())
      names(ranks) <- names
      self$data <- ranks
      invisible()
    },
    append = function(names, ranks = NULL) {
      new_ranks <- ranks %|||% rep(0L, length(names))
      ranks <- c(self$get_ranks(), new_ranks)
      names <- c(self$get_names(), names)
      names(ranks) <- names
      self$data <- ranks
      invisible()
    },
    should_dequeue = function() {
      any(as.integer(ceiling(self$get_ranks())) == 0L)
    },
    # Only necessary for parallel computations.
    increment_ranks = function(names, by) {
      index <- match(x = names(self$data), table = names, nomatch = 0L) > 0L
      self$data[index] <- .subset(self$data, index) + by
    },
    # Only necessary for parallel computations.
    update_ranks = function(target, scheduler) {
      names <- target_downstream_names(target, scheduler)
      target_decrement_ranks(names, scheduler)
    },
    # engraph_branches() is unnecessary in the sequential queue:
    # the queue only needs the graph for update_ranks() and increment_ranks(),
    # which in turn are only necessary when there are parallel computations.
    engraph_branches = function(target, pipeline, scheduler) {
      graph <- scheduler$graph
      graph$insert_edges(junction_upstream_edges(target$junction))
      edges <- data_frame(
        from = target_get_children(target),
        to = target_get_name(target)
      )
      graph$insert_edges(edges)
    },
    # Only necessary for parallel computations.
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
      if (!is.numeric(ranks) || anyNA(ranks) || any(ranks <= -1L)) {
        tar_throw_validate("ranks must be nonmissing numerics greater than -1.")
      }
    },
    validate = function() {
      self$validate_names(self$get_names())
      self$validate_ranks(self$get_ranks())
    }
  )
)
