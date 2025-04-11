mermaid_init <- function(
  network,
  label = NULL,
  label_break = "<br>",
  label_width = 30L,
  show_legend = TRUE,
  show_color = TRUE
) {
  mermaid_new(
    network = network,
    label = label,
    label_break = label_break,
    label_width = label_width,
    show_legend = show_legend,
    show_color = show_color
  )
}

mermaid_new <- function(
  network = NULL,
  label = NULL,
  label_break = NULL,
  label_width = NULL,
  show_legend = NULL,
  show_color = NULL
) {
  mermaid_class$new(
    network = network,
    label = label,
    label_break = label_break,
    label_width = label_width,
    show_legend = show_legend,
    show_color = show_color
  )
}

mermaid_class <- R6::R6Class(
  classname = "tar_mermaid",
  inherit = visual_class,
  class = FALSE,
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    show_legend = NULL,
    show_color = NULL,
    initialize = function(
      network = NULL,
      label = NULL,
      label_break = NULL,
      label_width = NULL,
      show_legend = NULL,
      show_color = NULL
    ) {
      super$initialize(
        network = network,
        label = label,
        label_break = label_break,
        label_width = label_width
      )
      self$show_legend <- show_legend
      self$show_color <- show_color
    },
    produce_class_defs = function() {
      vertices <- self$network$vertices
      status <- c(unique(vertices$status), "none")
      color <- self$produce_colors(status)
      fill <- self$produce_fills(status)
      sprintf(
        "  classDef %s stroke:#000000,color:%s,fill:%s;",
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
      type$name <- ordered(
        type$name,
        levels = c("object", "function", "stem", "pattern")
      )
      type <- type[order(type$name),, drop = FALSE] # nolint
      type$name <- as.character(type$name)
      type$open <- self$produce_shape_open(type$name)
      type$close <- self$produce_shape_close(type$name)
      legend <- rbind(status, type)
      legend$label <- gsub("uptodate", "Up to date", legend$name)
      legend <- legend[legend$label != "none",, drop = FALSE] # nolint
      legend$label[legend$label == "stem"] <- "Regular target"
      legend$label[legend$label == "pattern"] <- "Dynamic branches"
      legend$label <- capitalize(legend$label)
      legend
    },
    produce_mermaid_vertices_text = function(data) {
      sprintf(
        "%s%s%s%s:::%s",
        sprintf("x%s", as.character(map_chr(data$name, hash_object))),
        data$open,
        sprintf("\"%s\"", data$label),
        data$close,
        data$status
      )
    },
    produce_mermaid_lines_graph = function() {
      vertices <- self$network$vertices
      vertices$open <- self$produce_shape_open(vertices$type)
      vertices$close <- self$produce_shape_close(vertices$type)
      text <- self$produce_mermaid_vertices_text(vertices)
      names(text) <- self$network$vertices$name
      edges <- self$network$edges
      disconnected <- setdiff(vertices$name, c(edges$from, edges$to))
      disconnected <- unname(text[disconnected])
      edges$from <- unname(text[edges$from])
      edges$to <- unname(text[edges$to])
      out <- c(
        sprintf("    %s --> %s", edges$from, edges$to),
        paste0("    ", disconnected)
      )
      out <- c("  subgraph Graph", "    direction LR", out, "  end")
    },
    produce_mermaid_lines_legend = function() {
      vertices <- produce_mermaid_vertices_text(self$legend)
      c("  subgraph Legend", paste0("    ", vertices), "  end")
    },
    produce_graph_styles = function(show_legend) {
      c(
        if_any(
          show_legend,
          "  style Legend fill:#FFFFFF00,stroke:#000000;",
          character(0L)
        ),
        "  style Graph fill:#FFFFFF00,stroke:#000000;"
      )
    },
    produce_visual = function() {
      if (nrow(self$network$vertices) < 1L) {
        return("")
      }
      graph <- self$produce_mermaid_lines_graph()
      legend <- if_any(
        self$show_legend,
        self$produce_mermaid_lines_legend(),
        character(0)
      )
      class_defs <- if_any(
        self$show_color,
        self$produce_class_defs(),
        character(0)
      )
      graph_styles <- self$produce_graph_styles(self$show_legend)
      c(
        "graph LR",
        graph_styles,
        legend,
        graph,
        class_defs
      )
    },
    update_extra = function() {
    },
    validate = function() {
      super$validate()
      if (!is.null(self$visual)) {
        tar_assert_chr(self$visual)
      }
    }
  )
)
