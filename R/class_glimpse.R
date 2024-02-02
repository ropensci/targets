glimpse_init <- function(
  pipeline,
  meta = meta_init(),
  progress = progress_init(),
  targets_only = TRUE,
  names = NULL,
  shortcut = FALSE,
  allow = NULL,
  exclude = NULL
) {
  glimpse_new(
    pipeline = pipeline,
    meta = meta,
    progress = progress,
    targets_only = targets_only,
    names = names,
    shortcut = shortcut,
    allow = allow,
    exclude = exclude
  )
}

glimpse_new <- function(
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
  glimpse_class$new(
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

glimpse_class <- R6::R6Class(
  classname = "tar_glimpse",
  inherit = network_class,
  class = FALSE,
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    update_imports = function() {
      envir <- self$pipeline$imports
      graph <- graph_envir(envir)
      edges <- lapply(as_data_frame(igraph::as_edgelist(graph)), as.character)
      edges <- data_frame(from = edges[[1]], to = edges[[2]])
      edges <- edges[edges$from != edges$to,, drop = FALSE] # nolint
      names <- names(envir)
      type <- map_chr(names, type_import, envir = envir)
      status <- rep("none", length(names))
      vertices <- data_frame(
        name = names,
        type = type,
        status = status,
        seconds = rep(NA_real_, length(names)),
        bytes = rep(NA_real_, length(names)),
        branches = rep(NA_integer_, length(names))
      )
      self$edges_imports <- edges
      self$vertices_imports <- vertices
    },
    update_targets = function() {
      names <- pipeline_get_names(self$pipeline)
      type <- map_chr(names, function(name) {
        target_get_type(pipeline_get_target(pipeline, name))
      })
      status <- rep("none", length(names))
      vertices <- data_frame(
        name = names,
        type = type,
        status = status,
        seconds = rep(NA_real_, length(names)),
        bytes = rep(NA_real_, length(names)),
        branches = rep(NA_integer_, length(names))
      )
      names <- c(names, names(self$pipeline$imports))
      edges <- pipeline_upstream_edges(self$pipeline, targets_only = FALSE)
      edges <- edges[edges$from %in% names & edges$to %in% names,, drop = FALSE] # nolint
      edges <- edges[edges$from != edges$to,, drop = FALSE] # nolint
      self$edges_targets <- edges
      self$vertices_targets <- vertices
    }
  )
)
