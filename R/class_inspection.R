inspection_init <- function(
  pipeline,
  meta = meta_init(),
  progress = progress_init(),
  outdated = TRUE,
  reporter = "silent"
) {
  inspection_new(
    pipeline = pipeline,
    meta = meta,
    progress = progress,
    outdated = outdated,
    reporter = reporter
  )
}

inspection_new <- function(
  pipeline = NULL,
  meta = NULL,
  progress = NULL,
  outdated = NULL,
  reporter = NULL,
  vertices = NULL,
  edges = NULL,
  vertices_imports = NULL,
  edges_imports = NULL,
  vertices_targets = NULL,
  edges_targets = NULL
) {
  inspection_class$new(
    pipeline = pipeline,
    meta = meta,
    progress = progress,
    outdated = outdated,
    reporter = reporter,
    vertices = vertices,
    edges = edges,
    vertices_imports = vertices_imports,
    edges_imports = edges_imports,
    vertices_targets = vertices_targets,
    edges_targets = edges_targets
  )
}

inspection_class <- R6::R6Class(
  classname = "tar_inspection",
  inherit = network_class,
  class = FALSE,
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    pipeline = NULL,
    meta = NULL,
    progress = NULL,
    outdated = NULL,
    reporter = NULL,
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
      outdated = NULL,
      reporter = NULL,
      vertices = NULL,
      edges = NULL,
      vertices_imports = NULL,
      edges_imports = NULL,
      vertices_targets = NULL,
      edges_targets = NULL
    ) {
      super$initialize(
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
      self$outdated <- outdated
      self$reporter <- reporter
    },
    hashes_to_vertices = function(hashes) {
      data_frame(
        name = hashes$name,
        type = hashes$type,
        new = hashes$data
      )
    },
    produce_outdated = function(vertices) {
      outdated <- outdated_init(
        pipeline = self$pipeline,
        queue = "sequential",
        meta = self$meta,
        reporter = self$reporter
      )
      outdated$run()
      names_outdated <- counter_get_names(outdated$outdated)
      is_outdated <- vertices$name %in% names_outdated
      ifelse(is_outdated, "outdated", "uptodate")
    },
    resolve_import_status = function(vertices) {
      self$meta$database$ensure_preprocessed(write = FALSE)
      names <- fltr(vertices$name, ~self$meta$exists_record(.x))
      data <- map_chr(names, ~self$meta$get_record(.x)$data)
      meta <- data_frame(name = names, old = data)
      out <- merge(vertices, meta, all.x = TRUE)
      out$old[is.na(out$old)] <- ""
      out$status <- ifelse(out$new == out$old, "uptodate", "outdated")
      out$status <- as.character(out$status)
      out$status[is.na(out$status)] <- "dormant"
      out$seconds <- rep(NA_real_, nrow(out))
      out$bytes <- rep(NA_real_, nrow(out))
      out$branches <- rep(NA_integer_, nrow(out))
      out[, c("name", "type", "status", "seconds", "bytes", "branches")]
    },
    resolve_target_status = function(vertices) {
      vertices <- vertices[order(vertices$name),, drop = FALSE] # nolint
      status <- trn(
        self$outdated,
        self$produce_outdated(vertices),
        rep("dormant", nrow(vertices))
      )
      pipeline <- self$pipeline
      type <- map_chr(vertices$name, function(name) {
        target_get_type(pipeline_get_target(pipeline, name))
      })
      progress <- self$progress$database$read_condensed_data()
      if (self$outdated) {
        progress <- progress[progress$progress != "built",, drop = FALSE] # nolint
      }
      out <- merge(vertices, progress, all.x = TRUE, sort = FALSE)
      out <- out[order(out$name),, drop = FALSE] # nolint
      levels <- c("running", "built", "canceled", "errored")
      in_levels <- !is.na(out$progress) & out$progress %in% levels
      status <- ifelse(in_levels, out$progress, status)
      status[is.na(status)] <- "dormant"
      data_frame(name = vertices$name, type = type, status = status)
    },
    resolve_target_meta = function(vertices) {
      meta <- map(vertices$name, function(name) {
        if (self$meta$exists_record(name)) {
          record <- self$meta$get_record(name)
          data_frame(
            name = name,
            seconds = record$seconds,
            bytes = record$bytes,
            branches = trn(
              anyNA(record$children) || identical(record$type, "stem"),
              NA_integer_,
              length(record$children)
            )
          )
        }
      })
      meta <- do.call(rbind, meta) %||% data_frame(
        name = character(0),
        seconds = numeric(0),
        bytes = numeric(0),
        branches = integer(0)
      )
      merge(vertices, meta, all.x = TRUE, sort = FALSE)
    },
    update_imports = function() {
      envir <- self$pipeline$imports
      graph <- graph_envir(envir)
      edges <- lapply(as_data_frame(igraph::get.edgelist(graph)), as.character)
      edges <- data_frame(from = edges[[1]], to = edges[[2]])
      edges <- edges[edges$from != edges$to,, drop = FALSE] # nolint
      vertices <- self$hashes_to_vertices(hash_imports_graph(envir, graph))
      self$edges_imports <- edges
      vertices <- self$resolve_import_status(vertices)
      self$vertices_imports <- vertices
    },
    update_targets = function() {
      names <- pipeline_get_names(self$pipeline)
      vertices <- data_frame(name = names)
      vertices <- self$resolve_target_status(vertices)
      vertices <- self$resolve_target_meta(vertices)
      names <- c(names, names(self$pipeline$imports))
      edges <- pipeline_upstream_edges(self$pipeline, targets_only = FALSE)
      edges <- edges[edges$from %in% names & edges$to %in% names,, drop = FALSE] # nolint
      edges <- edges[edges$from != edges$to,, drop = FALSE] # nolint
      self$edges_targets <- edges
      self$vertices_targets <- vertices
    },
    validate = function() {
      super$validate()
      assert_lgl(self$outdated)
    }
  )
)
