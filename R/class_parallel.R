parallel_init <- function(names = character(0), ranks = integer(0)) {
  names(ranks) <- as.character(names)
  ranks <- sort(ranks)
  parallel_new(
    data = lookup_init(as.list(ranks)),
    head = names(ranks)[ranks <= 0],
    size = length(names)
  )
}

parallel_new <- function(data = NULL, head = NULL, size = NULL) {
  parallel_class$new(data = data, head = head, size = size)
}

parallel_class <- R6::R6Class(
  classname = "tar_parallel",
  inherit = queue_class,
  class = FALSE,
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    head = NULL,
    size = NULL,
    initialize = function(data = NULL, head = NULL, size = NULL) {
      super$initialize(data = data)
      self$head <- head
      self$size <- size
    },
    is_nonempty = function() {
      .subset2(self, "size") > 0L
    },
    dequeue = function() {
      head <- .subset2(self, "head")
      if (length(head)) {
        out <- head[1L]
        self$head <- head[-1L]
        self$size <- self$size - 1L
        return(out)
      }
      NULL
    },
    insert = function(names, ranks = NULL) {
      self$head <- c(names[ranks <= 0], .subset2(self, "head"))
      data <- .subset2(self, "data")
      lapply(seq_along(names), function(index) data[[names[index]]] <- ranks[index])
      self$size <- self$size + length(names)
    },
    should_dequeue = function() {
      length(.subset2(self, "head")) > 0L
    },
    # Only necessary for parallel computations.
    increment_ranks = function(names, by) {
      data <- .subset2(self, "data")
      ranks <- as.numeric(lapply(names, function(name) .subset2(data, name))) + by
      lapply(seq_along(names), function(index) data[[names[index]]] <- ranks[index])
      self$head <- c(self$head, names[ranks <= 0])
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
    # validate_names = function(names) {
    #   tar_assert_chr(names)
    #   if (anyNA(names) || anyDuplicated(names)) {
    #     tar_throw_validate("names must unique finite character strings.")
    #   }
    # },
    # validate_ranks = function(ranks) {
    #   if (!is.numeric(ranks) || anyNA(ranks) || any(ranks <= -1L)) {
    #     tar_throw_validate("ranks must be nonmissing numerics greater than -1.")
    #   }
    # },
    validate = function() {
      # self$validate_names(self$get_names())
      # self$validate_ranks(self$get_ranks())
    }
  )
)
