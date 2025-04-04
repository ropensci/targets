parallel_init <- function(names = character(0), ranks = integer(0)) {
  names <- as.character(names)
  names(ranks) <- names
  parallel_new(
    data = lookup_init(as.list(ranks)),
    ready = sequential_init(names(ranks)[ranks <= 0]),
    size = length(ranks)
  )
}

parallel_new <- function(data = NULL, ready = NULL, size = NULL) {
  parallel_class$new(data = data, ready = ready, size = size)
}

parallel_class <- R6::R6Class(
  classname = "tar_parallel",
  inherit = queue_class,
  class = FALSE,
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    ready = NULL,
    size = NULL,
    initialize = function(data = NULL, ready = NULL, size = NULL) {
      super$initialize(data = data)
      self$ready <- ready
      self$size <- size
    },
    is_nonempty = function() {
      .subset2(self, "size") > 0L
    },
    dequeue = function() {
      .subset2(.subset2(self, "ready"), "dequeue")()
    },
    insert = function(names, ranks = NULL) {
      self$size <- .subset2(self, "size") + length(names)
      is_ready <- ranks <= 0
      not_ready <- !is_ready
      if (any(is_ready)) {
        .subset2(.subset2(self, "ready"), "insert")(names = names[is_ready])
      }
      if (any(not_ready)) {
        index <- 1L
        names <- names[not_ready]
        ranks <- ranks[not_ready]
        n <- length(names)
        data <- .subset2(self, "data")
        while (index <= n) {
          data[[.subset(names, index)]] <- .subset(ranks, index)
          index <- index + 1L
        }
      }
    },
    should_dequeue = function() {
      .subset2(.subset2(self, "ready"), "should_dequeue")()
    },
    # Only necessary for parallel computations.
    increment_ranks = function(names, by) {
      index <- 1L
      n <- length(names)
      data <- .subset2(self, "data")
      ready <- .subset2(self, "ready")

if (length(by) == 1L) {
  by <- rep(by, n)
}


      while (index <= n) {
        name <- .subset(names, index)
        rank <- .subset2(data, name) + .subset(by, index)


        if (rank > 0) {
          data[[name]] <- rank
        } else {
          .subset2(ready, "insert")(names = name) # probably slow because it grows a vector.
        }
        index <- index + 1L
      }
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
