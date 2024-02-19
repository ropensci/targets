visual_new <- function(
  network = NULL,
  label = NULL,
  label_break = NULL,
  label_width = NULL
) {
  visual_class$new(
    network = network,
    label = label,
    label_break = label_break,
    label_width = label_width
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
    label_width = NULL,
    visual = NULL,
    initialize = function(
      network = NULL,
      label = NULL,
      label_break = NULL,
      label_width = NULL
    ) {
      self$network <- network
      self$label <- label
      self$label_break <- label_break
      self$label_width <- label_width
    },
    produce_colors = function(status) {
      colors <- c(
        completed = "#000000",
        uptodate = "#ffffff",
        outdated = "#000000",
        dispatched = "#000000",
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
        completed = "#E1BD6D",
        uptodate = "#354823",
        outdated = "#78B7C5",
        dispatched = "#DC863B",
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
      description <- vertices$description
      description[is.na(description)] <- ""
      seconds <- as.character(units_seconds(vertices$seconds))
      bytes <- as.character(units_bytes(vertices$bytes))
      branches <- as.character(units_branches(vertices$branches))
      if (!is.null(self$label_width)) {
        n <- self$label_width
        description <- truncate_character(description, n)
        seconds <- truncate_character(seconds, n)
        bytes <- truncate_character(bytes, n)
        branches <- truncate_character(branches, n)
      }
      out <- vertices$name
      if ("description" %in% self$label) {
        i <- nzchar(description)
        out[i] <- paste(out[i], description[i], sep = self$label_break)
      }
      if ("time" %in% self$label) {
        i <- nzchar(seconds)
        out[i] <- paste(out[i], seconds[i], sep = self$label_break)
      }
      if ("size" %in% self$label) {
        i <- nzchar(bytes)
        out[i] <- paste(out[i], bytes[i], sep = self$label_break)
      }
      if ("branches" %in% self$label) {
        i <- nzchar(branches)
        out[i] <- paste(out[i], branches[i], sep = self$label_break)
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
