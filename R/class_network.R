network_new <- function(
  pipeline = NULL,
  meta = NULL,
  progress = NULL,
  targets_only = NULL,
  names = NULL,
  shortcut = NULL,
  allow = NULL,
  exclude = NULL,
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
    targets_only = targets_only,
    names = names,
    shortcut = shortcut,
    allow = allow,
    exclude = exclude,
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
    targets_only = NULL,
    names = NULL,
    shortcut = NULL,
    allow = NULL,
    exclude = NULL,
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
      targets_only = NULL,
      names = NULL,
      shortcut = NULL,
      allow = NULL,
      exclude = NULL,
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
      self$targets_only <- targets_only
      self$names <- names
      self$shortcut <- shortcut
      self$allow <- allow
      self$exclude <- exclude
      self$vertices <- vertices
      self$edges <- edges
      self$vertices_imports <- vertices_imports
      self$edges_imports <- edges_imports
      self$vertices_targets <- vertices_targets
      self$edges_targets <- edges_targets
    },
    shortcut_vertices = function() {
      if (!self$shortcut) {
        return()
      }
      vertices <- self$vertices
      edges <- self$edges
      vertices <- vertices[vertices$name %in% self$names,, drop = FALSE] # nolint
      edges <- edges[edges$from %in% self$names,, drop = FALSE] # nolint
      edges <- edges[edges$to %in% self$names,, drop = FALSE] # nolint
      self$vertices <- vertices
      self$edges <- edges
    },
    allow_vertices = function() {
      vertices <- self$vertices
      allow <- tar_tidyselect_eval(self$allow, vertices$name)
      if (is.null(allow)) {
        return()
      }
      edges <- self$edges
      vertices <- vertices[vertices$name %in% allow,, drop = FALSE] # nolint
      edges <- edges[edges$from %in% allow,, drop = FALSE] # nolint
      edges <- edges[edges$to %in% allow,, drop = FALSE] # nolint
      self$vertices <- vertices
      self$edges <- edges
    },
    exclude_vertices = function() {
      vertices <- self$vertices
      exclude <- tar_tidyselect_eval(self$exclude, vertices$name)
      if (is.null(exclude)) {
        return()
      }
      edges <- self$edges
      vertices <- vertices[!(vertices$name %in% exclude),, drop = FALSE] # nolint
      edges <- edges[!(edges$from %in% exclude),, drop = FALSE] # nolint
      edges <- edges[!(edges$to %in% exclude),, drop = FALSE] # nolint
      self$vertices <- vertices
      self$edges <- edges
    },
    update = function() {
      pipeline_prune_names(self$pipeline, self$names)
      if (!self$targets_only) {
        self$update_imports()
      }
      self$update_targets()
      vertices <- rbind(self$vertices_targets, self$vertices_imports)
      vertices <- vertices[!duplicated(vertices$name),, drop = FALSE] # nolint
      vertices <- vertices[order(vertices$name),, drop = FALSE] # nolint
      edges <- rbind(self$edges_imports, self$edges_targets)
      edges <- edges[edges$from %in% vertices$name,, drop = FALSE] # nolint
      edges <- edges[edges$to %in% vertices$name,, drop = FALSE] # nolint
      edges <- edges[order(edges$from),, drop = FALSE] # nolint
      self$vertices <- vertices
      self$edges <- edges
      self$shortcut_vertices()
      self$allow_vertices()
      self$exclude_vertices()
    },
    validate = function() {
      tar_assert_identical_chr(class(self$pipeline)[1], "tar_pipeline")
      if (!is.null(self$meta)) {
        self$meta$validate()
      }
      if (!is.null(self$progress)) {
        self$progress$validate()
      }
      tar_assert_lgl(self$targets_only)
      if (!is.null(self$names)) {
        tar_assert_chr(self$names)
      }
      tar_assert_lgl(self$shortcut)
      tar_assert_scalar(self$shortcut)
      if (!is.null(self$allow)) {
        tar_assert_true(
          inherits(self$allow, "quosure") || is.character(self$allow)
        )
      }
      if (!is.null(self$exclude)) {
        tar_assert_true(
          inherits(self$exclude, "quosure") || is.character(self$exclude)
        )
      }
    }
  )
)
