parallel_init <- function(names = character(0), ranks = integer(0)) {
  data <- ranks
  min <- safe_min(ranks)
  names(data) <- as.character(names)
  parallel_new(data = sort(data), min = min)
}

parallel_new <- function(data = NULL, min = NULL) {
  parallel_class$new(data = data, min = min)
}

parallel_class <- R6::R6Class(
  classname = "tar_parallel",
  inherit = queue_class,
  class = FALSE,
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    min = NULL,
    initialize = function(data = NULL, min = NULL) {
      super$initialize(data)
      self$min <- min
    },
    is_nonempty = function() {
      length(.subset2(self, "data")) > 0L
    },
    get_names = function() {
      names(.subset2(self, "data"))
    },
    get_ranks = function() {
      unname(.subset2(self, "data"))
    },
    sort_data = function() {
      self$data <- sort(.subset2(self, "data"))
    },
    update_min = function() {
      data <- .subset2(self, "data")
      if (length(data)) {
        self$min <- as.numeric(data[1L])
      } else {
        self$min <- Inf
      }
    },
    dequeue = function() {
      head <- names(.subset2(self, "data"))[1L]
      self$data <- .subset2(self, "data")[-1L]
      .subset2(self, "update_min")()
      head
    },
    prepend = function(names, ranks = NULL) {
      new_ranks <- ranks %|||% rep(0L, length(names))
      ranks <- c(new_ranks, self$get_ranks())
      names <- c(names, self$get_names())
      names(ranks) <- names
      self$data <- ranks
      .subset2(self, "sort_data")()
      .subset2(self, "update_min")()
      invisible()
    },
    append = function(names, ranks = NULL) {
      new_ranks <- ranks %|||% rep(0L, length(names))
      ranks <- c(self$get_ranks(), new_ranks)
      names <- c(self$get_names(), names)
      names(ranks) <- names
      self$data <- ranks
      .subset2(self, "sort_data")()
      .subset2(self, "update_min")()
      invisible()
    },
    should_dequeue = function() {
      min <- .subset2(self, "min")
      length(min) && min <= 0
    },
    # Only necessary for parallel computations.
    increment_ranks = function(names, by) {
      data <- .subset2(self, "data")
      index <- match(x = names(data), table = names, nomatch = 0L) > 0L
      new_ranks <- .subset(data, index) + by
      self$data[index] <- new_ranks
      .subset2(self, "sort_data")()
      decremented <- by < 0
      if (any(decremented)) {
        self$min <- safe_min(c(self$min, safe_min(new_ranks[decremented])))
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
      tar_assert_dbl(self$min)
      self$validate_names(self$get_names())
      self$validate_ranks(self$get_ranks())
    }
  )
)
