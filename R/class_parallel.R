# The parallel queue is a hybrid data structure to promote efficiency
# (c.f. https://github.com/ropensci/targets/issues/1458).
# Targets that are ready to run are stored in a sequential
# queue where they can be popped efficiently.
# Targets that are waiting for dependencies are stored in a hash environment
# so their ranks can be decremented in constant time.
parallel_init <- function(
  names = character(0),
  ranks = numeric(0L),
  step = 1e3
) {
  names(ranks) <- as.character(names)
  positive_rank <- ranks > 0
  data <- lookup_init(as.list(ranks[positive_rank]))
  ready <- sequential_init(names = names[!positive_rank], step = step)
  parallel_new(data = data, n_data = sum(positive_rank), ready = ready)
}

parallel_new <- function(data = NULL, n_data = NULL, ready = NULL) {
  parallel_class$new(data = data, n_data = n_data, ready = ready)
}

parallel_class <- R6::R6Class(
  classname = "tar_parallel",
  inherit = queue_class,
  class = FALSE,
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    n_data = NULL,
    ready = NULL,
    initialize = function(data = NULL, n_data = NULL, ready = NULL) {
      super$initialize(data = data)
      self$n_data <- n_data
      self$ready <- ready
    },
    is_nonempty = function() {
      .subset2(self, "n_data") > 0L ||
        .subset2(.subset2(self, "ready"), "is_nonempty")()
    },
    dequeue = function() {
      .subset2(.subset2(self, "ready"), "dequeue")()
    },
    should_dequeue = function() {
      .subset2(.subset2(self, "ready"), "should_dequeue")()
    },
    insert = function(names, ranks = NULL, method = "prepend") {
      ready <- .subset2(self, "ready")
      if (is.null(ranks)) {
        .subset2(ready, method)(names = names)
        return()
      }
      positive_rank <- ranks > 0
      .subset2(ready, method)(names = names[!positive_rank])
      index <- 1L
      names <- names[positive_rank]
      ranks <- ranks[positive_rank]
      n <- length(names)
      data <- .subset2(self, "data")
      new <- 0L
      while (index <= n) {
        name <- .subset(names, index)
        new <- new + is.null(.subset2(data, name))
        data[[name]] <- .subset(ranks, index)
        index <- index + 1L
      }
      self$n_data <- .subset2(self, "n_data") + new
    },
    append = function(names, ranks = NULL) {
      .subset2(self, "insert")(names = names, ranks = ranks, method = "append")
    },
    prepend = function(names, ranks = NULL) {
      .subset2(self, "insert")(names = names, ranks = ranks, method = "prepend")
    },
    reset = function() {
      .subset2(.subset2(self, "ready"), "reset")()
      self$data <- lookup_new()
      self$n_data <- 0L
    },
    # Only necessary for parallel computations.
    increment_ranks = function(names, by) {
      if (length(by) == 1L) {
        by <- rep(by, length(names))
      }
      index <- 1L
      n <- length(names)
      data <- .subset2(self, "data")
      ready_append <- .subset2(.subset2(self, "ready"), "append")
      while (index <= n) {
        name <- .subset(names, index)
        rank <- .subset2(data, name)
        if (is.null(rank)) {
          index <- index + 1L
          next
        }
        rank <- rank + .subset(by, index)
        if (rank <= 0) {
          ready_append(name)
          rm(list = name, envir = data)
          self$n_data <- .subset2(self, "n_data") - 1L
        } else {
          data[[name]] <- rank
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
    validate = function() {
      tar_assert_envir(self$data)
      tar_assert_int(self$n_data)
      self$ready$validate()
    }
  )
)
