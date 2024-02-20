visnetwork_init <- function(
  network,
  label = NULL,
  label_break = "\n",
  label_width = 30L,
  level_separation = NULL,
  degree_from = 1L,
  degree_to = 1L,
  zoom_speed = 1,
  physics = FALSE
) {
  visnetwork_new(
    network = network,
    label = label,
    label_break = label_break,
    label_width = label_width,
    level_separation = level_separation,
    degree_from = degree_from,
    degree_to = degree_to,
    zoom_speed = zoom_speed,
    physics = physics
  )
}

visnetwork_new <- function(
  network = NULL,
  label = NULL,
  label_break = NULL,
  label_width = NULL,
  level_separation = NULL,
  degree_from = NULL,
  degree_to = NULL,
  zoom_speed = NULL,
  physics = NULL
) {
  visnetwork_class$new(
    network = network,
    label = label,
    label_break = label_break,
    label_width = label_width,
    level_separation = level_separation,
    degree_from = degree_from,
    degree_to = degree_to,
    zoom_speed = zoom_speed,
    physics = physics
  )
}

visnetwork_class <- R6::R6Class(
  classname = "tar_visnetwork",
  inherit = visual_class,
  class = FALSE,
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    level_separation = NULL,
    degree_from = NULL,
    degree_to = NULL,
    zoom_speed = NULL,
    physics = NULL,
    initialize = function(
      network = NULL,
      label = NULL,
      label_break = NULL,
      label_width = NULL,
      level_separation = NULL,
      degree_from = NULL,
      degree_to = NULL,
      zoom_speed = NULL,
      physics = NULL
    ) {
      super$initialize(
        network = network,
        label = label,
        label_break = label_break,
        label_width = label_width
      )
      self$level_separation <- level_separation
      self$degree_from <- degree_from
      self$degree_to <- degree_to
      self$zoom_speed <- zoom_speed
      self$physics <- physics
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
    produce_visual = function() {
      tar_assert_package("visNetwork")
      vertices <- self$network$vertices
      edges <- self$network$edges
      out <- visNetwork::visNetwork(nodes = vertices, edges = edges, main = "")
      out <- visNetwork::visNodes(out, physics = self$physics)
      out <- visNetwork::visEdges(
        out,
        smooth = list(type = "cubicBezier", forceDirection = "horizontal")
      )
      out <- visNetwork::visOptions(
        graph = out,
        collapse = TRUE,
        highlightNearest = list(
          enabled = TRUE,
          algorithm = "hierarchical",
          degree = list(
            from = min(self$degree_from, nrow(vertices)),
            to = min(self$degree_to, nrow(vertices))
          )
        )
      )
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
      out <- visNetwork::visInteraction(
        graph = out,
        zoomSpeed = self$zoom_speed
      )
      visNetwork::visHierarchicalLayout(
        graph = out,
        direction = "LR",
        levelSeparation = self$level_separation
      )
    },
    update_ids = function() {
      self$network$vertices$id <- self$network$vertices$name
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
    update_shapes = function() {
      vertices <- self$network$vertices
      vertices$shape <- self$produce_shapes(vertices$type)
      self$network$vertices <- vertices
    },
    update_extra = function() {
      self$update_ids()
      self$update_positions()
      self$update_arrows()
      self$update_shapes()
    },
    validate = function() {
      super$validate()
      if (!is.null(self$visual)) {
        tar_assert_identical(class(self$visual)[1], "visNetwork")
      }
      tar_assert_scalar(self$degree_from)
      tar_assert_scalar(self$degree_to)
      tar_assert_dbl(self$degree_from)
      tar_assert_dbl(self$degree_to)
      tar_assert_ge(self$degree_from, 0L)
      tar_assert_ge(self$degree_to, 0L)
      tar_assert_scalar(self$zoom_speed)
      tar_assert_dbl(self$zoom_speed)
      tar_assert_positive(self$zoom_speed)
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
