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
    produce_legend = function() {
      status <- tibble::tibble(
        name = unique(self$network$vertices$status),
        open = "([",
        close = "])"
      )
      status$status <- status$name
      type <- tibble::tibble(
        name = unique(self$network$vertices$type),
        status = "none"
      )
      type$open <- self$produce_shape_open(type$name)
      type$close <- self$produce_shape_close(type$name)
      legend <- rbind(status, type)
      legend$label <- gsub("uptodate", "Up to date", legend$name)
      legend$label <- capitalize(legend$label)
      legend
    },
    produce_mermaid_vertices = function(data) {
      sprintf(
        "%s%s%s%s:::%s",
        data$name,
        data$open,
        data$label,
        data$close,
        data$status
      )
    },
    produce_mermaid_vertices_graph = function(side) {
      out <- self$network$edges
      out[[setdiff(colnames(out), side)]] <- NULL
      out$name <- out[[side]]
      out[[side]] <- NULL
      vertices <- self$network$vertices
      out <- merge(x = out, y = vertices, all = FALSE, sort = FALSE)
      out$open <- self$produce_shape_open(out$type)
      out$close <- self$produce_shape_close(out$type)
      self$produce_mermaid_vertices(out)
    },
    produce_mermaid_lines_graph = function() {
      from <- produce_mermaid_vertices_graph(side = "from")
      to <- produce_mermaid_vertices_graph(side = "to")
      sprintf("  %s --> %s", from, to)
    },
    produce_mermaid_lines_legend = function() {
      vertices <- produce_mermaid_vertices(self$legend)
      if (!length(vertices)) {
        return("")
      }
      if (length(vertices) == 1L) {
        vertices <- c(vertices, vertices)
      }
      from <- vertices[-length(vertices)]
      to <- vertices[-1]
      edges <- sprintf("%s --- %s", from, to)
      styles <- sprintf(
        "linkStyle %s stroke-width:0px,fill:none;",
        seq_along(edges) - 1
      )
      out <- paste0("    ", c(edges, styles))
      c("  subgraph Legend", out, "  end")
    },
    produce_visual = function() {
      classdefs <- produce_classdefs()
      graph <- produce_mermaid_lines_graph()
      legend <- produce_mermaid_lines_legend()
      out <- c(classdefs, legend, graph)
      if (length(out) > 0L && any(nzchar(out))) {
        out <- c("graph LR", out)
      }
      paste(out, collapse = "\n")
    },
    update_extra = function() {
    },
    validate = function() {
      super$validate()
      if (!is.null(self$visual)) {
        tar_assert_scalar(self$visual)
        tar_assert_chr(self$visual)
      }
    }
  )
)
