visual_new <- function(
  network = NULL,
  targets_only = NULL,
  allow = NULL,
  exclude = NULL
) {
  visual_class$new(network, targets_only, allow, exclude)
}

visual_class <- R6::R6Class(
  classname = "tar_visual",
  class = FALSE,
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    network = NULL,
    targets_only = NULL,
    allow = NULL,
    exclude = NULL,
    initialize = function(
      network = NULL,
      targets_only = NULL,
      allow = NULL,
      exclude = NULL
    ) {
      self$network <- network
      self$targets_only <- targets_only
      self$allow <- allow
      self$exclude <- exclude
    },
    allow_vertices = function() {
      allow <- self$allow
      if (is.null(allow)) {
        return()
      }
      vertices <- self$network$vertices
      edges <- self$network$edges
      vertices <- vertices[vertices$name %in% allow,, drop = FALSE] # nolint
      edges <- edges[edges$from %in% allow,, drop = FALSE] # nolint
      edges <- edges[edges$to %in% allow,, drop = FALSE] # nolint
      self$network$vertices <- vertices
      self$network$edges <- edges
    },
    exclude_vertices = function() {
      exclude <- self$exclude
      if (is.null(exclude)) {
        return()
      }
      vertices <- self$network$vertices
      edges <- self$network$edges
      vertices <- vertices[!(vertices$name %in% exclude),, drop = FALSE] # nolint
      edges <- edges[!(edges$from %in% exclude),, drop = FALSE] # nolint
      edges <- edges[!(edges$to %in% exclude),, drop = FALSE] # nolint
      self$network$vertices <- vertices
      self$network$edges <- edges
    },
    update_network = function() {
      self$network$update(targets_only = self$targets_only)
      self$allow_vertices()
      self$exclude_vertices()
    },
    validate = function() {
      self$network$validate()
      assert_lgl(self$targets_only)
      if (!is.null(self$allow)) {
        assert_chr(self$allow)
      }
      if (!is.null(self$exclude)) {
        assert_chr(self$exclude)
      }
      invisible()
    }
  )
)
