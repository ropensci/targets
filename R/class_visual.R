visual_new <- function(
  network = NULL,
  label = NULL,
  label_break = NULL
) {
  visual_class$new(
    network = network,
    label = label,
    label_break = label_break
  )
}

visual_class <- R6::R6Class(
  classname = "tar_visual",
  class = FALSE,
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    network = NULL,
    legend = NULL,
    label = NULL,
    label_break = NULL,
    visual = NULL,
    initialize = function(
      network = NULL,
      label = NULL,
      label_break = NULL
    ) {
      self$network <- network
      self$label <- label
      self$label_break <- label_break
    },
    produce_colors = function(status) {
      colors <- c(
        built = "#000000",
        uptodate = "#ffffff",
        outdated = "#000000",
        started = "#000000",
        canceled = "#000000",
        errored = "#ffffff",
        queued = "#000000",
        skipped = "#ffffff",
        none = "#000000"
      )
      unname(colors[status])
    },
    produce_fills = function(status) {
      fills <- c(
        built = "#E1BD6D",
        uptodate = "#354823",
        outdated = "#78B7C5",
        started = "#DC863B",
        canceled = "#FAD510",
        errored = "#C93312",
        queued = "#D2D2D0",
        skipped = "#7500D1",
        none = "#94a4ac"
      )
      unname(fills[status])
    },
    produce_labels = function() {
      vertices <- self$network$vertices
      seconds <- units_seconds(vertices$seconds)
      bytes <- units_bytes(vertices$bytes)
      branches <- units_branches(vertices$branches)
      out <- vertices$name
      if ("time" %in% self$label) {
        out <- paste(out, seconds, sep = self$label_break)
      }
      if ("size" %in% self$label) {
        out <- paste(out, bytes, sep = self$label_break)
      }
      if ("branches" %in% self$label) {
        out <- paste(out, branches, sep = self$label_break)
      }
      out
    },
    update_colors = function() {
      vertices <- self$network$vertices
      vertices$color <- self$produce_fills(vertices$status)
      self$network$vertices <- vertices
    },
    update_legend = function() {
      self$legend <- self$produce_legend()
    },
    update_network = function() {
      self$network$update()
    },
    update_visual = function() {
      self$visual <- self$produce_visual()
    },
    update_labels = function() {
      self$network$vertices$label <- self$produce_labels()
    },
    update = function() {
      self$update_network()
      self$update_labels()
      self$update_colors()
      self$update_extra()
      self$update_legend()
      self$update_visual()
    },
    validate = function() {
      self$network$validate()
      tar_assert_in(self$label, c("time", "size", "branches"))
      tar_assert_scalar(self$label_break)
      tar_assert_chr(self$label_break)
      tar_assert_nzchar(self$label_break)
      if (!is.null(self$legend)) {
        tar_assert_df(self$legend)
      }
      invisible()
    }
  )
)
