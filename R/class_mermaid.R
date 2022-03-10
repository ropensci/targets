mermaid_init <- function(
  network,
  label = NULL,
  label_break = "<br>"
) {
  mermaid_new(
    network = network,
    label = label,
    label_break = label_break
  )
}

mermaid_new <- function(
  network = NULL,
  label = NULL,
  label_break = NULL
) {
  mermaid_class$new(
    network = network,
    label = label,
    label_break = label_break
  )
}

mermaid_class <- R6::R6Class(
  classname = "tar_mermaid",
  inherit = visual_class,
  class = FALSE,
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    produce_classdefs = function() {
      vertices <- self$network$vertices
      status <- unique(vertices$status)
      color <- self$produce_colors(status)
      fill <- self$produce_fills(status)
      sprintf(
        "  classDef %s stroke-width:0px,color:%s,fill:%s;",
        status,
        color,
        fill
      )
    },
    produce_legend = function() {
      NULL
    },
    produce_shape_open = function(type) {
      open <- c("{{", ">", "([", "[")
      names(open) <- c("object", "function", "stem", "pattern")
      unname(open[type])
    },
    produce_shape_close = function(type) {
      open <- c("}}", "]", "])", "]")
      names(open) <- c("object", "function", "stem", "pattern")
      unname(open[type])
    },
    produce_main_nodes = function(side) {
      out <- self$network$edges
      out[[setdiff(colnames(out), side)]] <- NULL
      out$name <- out[[side]]
      out[[side]] <- NULL
      vertices <- self$network$vertices
      out <- merge(x = out, y = vertices, all = FALSE, sort = FALSE)
      out$open <- self$produce_shape_open(out$type)
      out$close <- self$produce_shape_close(out$type)
      sprintf(
        "%s%s%s%s:::%s",
        out$name,
        out$open,
        out$label,
        out$close,
        out$status
      )
    },
    produce_visual = function() {
      classdefs <- produce_classdefs()
      from <- produce_main_nodes(side = "from")
      to <- produce_main_nodes(side = "to")
      edges <- sprintf("  %s --> %s", from, to)
      c("graph LR", classdefs, edges)
    },
    update_extra = function() {
    },
    validate = function() {
      super$validate()
      if (!is.null(self$visual)) {
        tar_assert_scalar(self$visual)
        tar_assert_chr(self$visual)
        tar_assert_nzchar(self$visual)
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
