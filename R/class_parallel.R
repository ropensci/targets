parallel_init <- function(names = character(0), ranks = integer(0)) {
  names <- as.character(names)
  names(ranks) <- names
  parallel_new(data = lookup_init(as.list(ranks)), queue = names[order(ranks)])
}

parallel_new <- function(data = NULL, queue = NULL) {
  parallel_class$new(data = data, queue = queue)
}

parallel_class <- R6::R6Class(
  classname = "tar_parallel",
  inherit = queue_class,
  class = FALSE,
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    queue = NULL,
    initialize = function(data = NULL, queue = NULL) {
      super$initialize(data = data)
      self$queue <- queue
    },
    is_nonempty = function() {
      length(.subset2(self, "queue")) > 0L
    },
    dequeue = function() {
      queue <- .subset2(self, "queue")
      if (length(queue) < 1L) {
        return(NULL)
      }
      head <- queue[1L]
      if (.subset2(.subset2(self, "data"), head) <= 0) {
        self$queue <- queue[-1L]
        return(head)
      } else {
        return(NULL)
      }
    },
    insert = function(names, ranks = NULL) {
      new_ranks <- ranks %|||% rep(0L, length(names))
      ranks <- c(new_ranks, self$get_ranks())
      names <- c(names, self$get_names())
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
