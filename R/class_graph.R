graph_init <- function(edges = NULL) {
  edges <- edges %|||% data_frame(from = character(0), to = character(0))
  upstream <- adjacency_list(edges$from, edges$to)
  downstream <- adjacency_list(edges$to, edges$from)
  graph_new(upstream, downstream)
}

graph_new <- function(upstream = NULL, downstream = NULL) {
  graph_class$new(upstream, downstream)
}

graph_class <- R6::R6Class(
  classname = "tar_queue",
  class = FALSE,
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    upstream = NULL,
    downstream = NULL,
    initialize = function(
      upstream = NULL,
      downstream = NULL
    ) {
      self$upstream <- upstream
      self$downstream <- downstream
    },
    produce_degrees = function(names, mode) {
      list <- trn(
        identical(mode, "upstream"),
        self$upstream,
        self$downstream
      )
      unname(map_int(list[names], length))
    },
    produce_upstream = function(name) {
      as.character(self$upstream[[name]])
    },
    produce_downstream = function(name) {
      as.character(self$downstream[[name]])
    },
    replace_upstream = function(name, from, to) {
      upstream <- self$upstream
      upstream[[name]][upstream[[name]] == from] <- to
      self$upstream <- upstream
    },
    insert_edges = function(edges) {
      upstream <- join_edges(self$upstream, edges$from, edges$to)
      downstream <- join_edges(self$downstream, edges$to, edges$from)
      self$upstream <- upstream
      self$downstream <- downstream
      invisible()
    },
    validate = function() {
      lapply(self$upstream, assert_chr)
      lapply(self$downstream, assert_chr)
      invisible()
    }
  )
)

adjacency_list <- function(from, to) {
  tapply(from, to, identity, simplify = FALSE)
}

join_edges <- function(edgelist, from, to) {
  new_edgelist <- adjacency_list(from, to)
  dups <- intersect(names(new_edgelist), names(edgelist))
  new <- setdiff(names(new_edgelist), names(edgelist))
  out <- c(edgelist, new_edgelist[new])
  for (name in dups) {
    out[[name]] <- union(out[[name]], new_edgelist[[name]])
  }
  out
}

remove_loops <- function(edges) {
  edges[edges$from != edges$to, ]
}
