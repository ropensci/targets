visnetwork_init <- function(
  network,
  targets_only = FALSE,
  allow = NULL,
  exclude = NULL,
  label = NULL,
  level_separation = NULL
) {
  visnetwork_new(
    network = network,
    targets_only = targets_only,
    allow = allow,
    exclude = exclude,
    label = label,
    level_separation = level_separation
  )
}

visnetwork_new <- function(
  network = NULL,
  targets_only = NULL,
  allow = NULL,
  exclude = NULL,
  label = NULL,
  legend = NULL,
  level_separation = NULL,
  visnetwork = NULL
) {
  visnetwork_class$new(
    network = network,
    targets_only = targets_only,
    allow = allow,
    exclude = exclude,
    label = label,
    legend = legend,
    level_separation = level_separation,
    visnetwork = visnetwork
  )
}

visnetwork_class <- R6::R6Class(
  classname = "tar_visnetwork",
  inherit = visual_class,
  class = FALSE,
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    network = NULL,
    targets_only = NULL,
    allow = NULL,
    exclude = NULL,
    label = NULL,
    legend = NULL,
    level_separation = NULL,
    visnetwork = NULL,
    initialize = function(
      network = NULL,
      targets_only = NULL,
      allow = NULL,
      exclude = NULL,
      label = NULL,
      legend = NULL,
      visnetwork = NULL,
      level_separation = NULL
    ) {
      super$initialize(
        network = network,
        targets_only = targets_only,
        allow = allow,
        exclude = exclude
      )
      self$label <- label
      self$legend <- legend
      self$level_separation <- level_separation
      self$visnetwork <- visnetwork
    },
    produce_colors = function(status) {
      colors <- c(
        built = "#E1BD6D",
        uptodate = "#354823",
        outdated = "#78B7C5",
        running = "#DC863B",
        canceled = "#FAD510",
        errored = "#C93312",
        dormant = "#9986A5",
        none = "#94a4ac"
      )
      colors[status]
    },
    produce_shapes = function(type) {
      shapes <- c(
        object = "triangleDown",
        `function` = "triangle",
        stem = "dot",
        pattern = "square"
      )
      shapes[type]
    },
    produce_labels = function(vertices) {
      vertices$name
    },
    produce_legend = function() {
      vertices <- self$network$vertices
      colors <- vertices[vertices$status != "none", c("status", "color")]
      shapes <- vertices[, c("type", "shape")]
      colors <- colors[!duplicated(colors),, drop = FALSE] # nolint
      shapes <- shapes[!duplicated(shapes),, drop = FALSE] # nolint
      colors$shape <- rep("dot", nrow(colors))
      shapes$color <- rep("#899DA4", nrow(shapes))
      colnames(colors) <- c("label", "color", "shape")
      colnames(shapes) <- c("label", "shape", "color")
      legend <- rbind(colors, shapes)
      rownames(legend) <- NULL
      legend$label <- gsub("uptodate", "Up to date", legend$label)
      legend$label <- capitalize(legend$label)
      legend
    },
    produce_visnetwork = function() {
      assert_package("visNetwork")
      vertices <- self$network$vertices
      edges <- self$network$edges
      vertices <- self$update_label(vertices)
      out <- visNetwork::visNetwork(nodes = vertices, edges = edges, main = "")
      out <- visNetwork::visNodes(out, physics = FALSE)
      out <- visNetwork::visEdges(
        out,
        smooth = list(type = "cubicBezier", forceDirection = "horizontal")
      )
      out <- visNetwork::visOptions(out, collapse = TRUE)
      out <- visNetwork::visLegend(
        graph = out,
        useGroups = FALSE,
        addNodes = self$legend,
        ncol = 1L,
        position = "right"
      )
      out <- visNetwork::visPhysics(
        graph = out,
        stabilization = FALSE
      )
      visNetwork::visHierarchicalLayout(
        graph = out,
        direction = "LR",
        levelSeparation = self$level_separation
      )
    },
    update_label = function(vertices) {
      seconds <- format_seconds(vertices$seconds)
      bytes <- format_bytes(vertices$bytes)
      branches <- format_branches(vertices$branches)
      if ("time" %in% label) {
        vertices$label <- paste(vertices$label, seconds, sep = "\n")
      }
      if ("size" %in% label) {
        vertices$label <- paste(vertices$label, bytes, sep = "\n")
      }
      if ("branches" %in% label) {
        vertices$label <- paste(vertices$label, branches, sep = "\n")
      }
      vertices
    },
    update_visnetwork = function() {
      self$visnetwork <- self$produce_visnetwork()
    },
    update_labels = function() {
      vertices <- self$network$vertices
      vertices$id <- vertices$name
      vertices$label <- self$produce_labels(vertices)
      self$network$vertices <- vertices
    },
    update_arrows = function() {
      edges <- self$network$edges
      edges$arrows <- rep("to", nrow(edges))
      self$network$edges <- edges
    },
    update_positions = function() {
      vertices <- self$network$vertices
      if (!nrow(vertices)) {
        return()
      }
      vertices <- position_level(vertices, self$network$edges)
      self$network$vertices <- vertices
    },
    update_colors = function() {
      vertices <- self$network$vertices
      vertices$color <- self$produce_colors(vertices$status)
      self$network$vertices <- vertices
    },
    update_shapes = function() {
      vertices <- self$network$vertices
      vertices$shape <- self$produce_shapes(vertices$type)
      self$network$vertices <- vertices
    },
    update_legend = function() {
      self$legend <- self$produce_legend()
    },
    update = function() {
      self$update_network()
      self$update_labels()
      self$update_positions()
      self$update_arrows()
      self$update_colors()
      self$update_shapes()
      self$update_legend()
      self$update_visnetwork()
    },
    validate = function() {
      super$validate()
      assert_in(self$label, c("time", "size", "branches"))
      if (!is.null(self$legend)) {
        assert_df(self$legend)
      }
      if (!is.null(self$visnetwork)) {
        assert_identical(class(self$visnetwork)[1], "visNetwork")
      }
    }
  )
)

position_level <- function(vertices, edges) {
  level <- 0L
  vertices$level <- rep(level, nrow(vertices))
  if (!nrow(vertices) || !nrow(edges)) {
    return(vertices)
  }
  igraph <- igraph::graph_from_data_frame(edges)
  while (length(igraph::V(igraph))) {
    level <- level + 1L
    leaves <- igraph_leaves(igraph)
    vertices[vertices$name %in% leaves, "level"] <- level
    igraph <- igraph::delete_vertices(graph = igraph, v = leaves)
  }
  vertices
}
