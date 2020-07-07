network_new <- function(
  pipeline = NULL,
  meta = NULL,
  progress = NULL,
  vertices = NULL,
  edges = NULL,
  vertices_imports = NULL,
  edges_imports = NULL,
  vertices_targets = NULL,
  edges_targets = NULL
) {
  network_class$new(
    pipeline = pipeline,
    meta = meta,
    progress = progress,
    vertices = vertices,
    edges = edges,
    vertices_imports = vertices_imports,
    edges_imports = edges_imports,
    vertices_targets = vertices_targets,
    edges_targets = edges_targets
  )
}

network_class <- R6::R6Class(
  classname = "tar_network",
  class = FALSE,
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    pipeline = NULL,
    meta = NULL,
    progress = NULL,
    vertices = NULL,
    edges = NULL,
    vertices_imports = NULL,
    edges_imports = NULL,
    vertices_targets = NULL,
    edges_targets = NULL,
    initialize = function(
      pipeline = NULL,
      meta = NULL,
      progress = NULL,
      vertices = NULL,
      edges = NULL,
      vertices_imports = NULL,
      edges_imports = NULL,
      vertices_targets = NULL,
      edges_targets = NULL
    ) {
      self$pipeline <- pipeline
      self$meta <- meta
      self$progress <- progress
      self$vertices <- vertices
      self$edges <- edges
      self$vertices_imports <- vertices_imports
      self$edges_imports <- edges_imports
      self$vertices_targets <- vertices_targets
      self$edges_targets <- edges_targets
    },
    update = function(targets_only = FALSE) {
      if (!targets_only) {
        self$update_imports()
      }
      self$update_targets()
      vertices <- rbind(self$vertices_targets, self$vertices_imports)
      vertices <- vertices[!duplicated(vertices$name), ]
      edges <- rbind(self$edges_imports, self$edges_targets)
      edges <- edges[edges$from %in% vertices$name,, drop = FALSE] # nolint
      edges <- edges[edges$to %in% vertices$name,, drop = FALSE] # nolint
      self$vertices <- vertices
      self$edges <- edges
    },
    validate = function() {
      assert_identical_chr(class(self$pipeline)[1], "tar_pipeline")
      if (!is.null(self$meta)) {
        self$meta$validate()
      }
      if (!is.null(self$progress)) {
        self$progress$validate()
      }
    }
  )
)
