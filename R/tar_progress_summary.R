#' @title Summarize target progress.
#' @export
#' @description Summarize the progress of a run of the pipeline.
#' @return A data frame with one row and the following optional columns that can be
#'   selected with `fields`. (`time` is omitted by default.)
#'   * `started`: number of targets that started and did not (yet) finish.
#'   * `built`: number of targets that completed without error or cancellation.
#'   * `errored`: number of targets that threw an error.
#'   * `canceled`: number of canceled targets (see [tar_cancel()]).
#'   * `since`: how long ago progress last changed (`Sys.time() - time`).
#'   * `time`: the time when the progress last changed
#'     (modification timestamp of the `_targets/meta/progress` file).
#' @param fields Optional, names of progress data columns to read.
#'   Set to `NULL` to read all fields.
#' @examples
#' if (identical(Sys.getenv("TAR_LONG_EXAMPLES"), "true")) {
#' tar_dir({ # tar_dir() runs code from a temporary directory.
#' tar_script({
#'   list(
#'     tar_target(x, seq_len(2)),
#'     tar_target(y, x, pattern = map(x)),
#'     tar_target(z, stopifnot(y < 1.5), pattern = map(y), error = "continue")
#'   )
#' }, ask = FALSE)
#' try(tar_make())
#' tar_progress_summary()
#' })
#' }
tar_progress_summary <- function(
  fields = c("started", "built", "errored", "canceled", "since")
) {
  assert_store()
  assert_path(path_progress())
  time <- file.mtime(path_progress())
  progress <- tibble::as_tibble(progress_init()$database$read_condensed_data())
  progress <- progress[progress$type != "pattern",, drop = FALSE] # nolint
  out <- tibble::tibble(
    started = sum(progress$progress == "started"),
    built = sum(progress$progress == "built"),
    errored = sum(progress$progress == "errored"),
    canceled = sum(progress$progress == "canceled"),
    since = format_seconds(difftime(Sys.time(), time, units = "secs")),
    time = time_stamp(time)
  )
  fields_quosure <- rlang::enquo(fields)
  fields <- eval_tidyselect(fields_quosure, colnames(out)) %|||% colnames(out)
  out[, fields, drop = FALSE]
}

tar_progress_branches_summary <- function(progress) {
  branches <- progress[progress$type == "branch",, drop = FALSE] # nolint
  group <- paste(branches$parent, branches$progress)
  table <- table(group)
  group <- names(table)
  long <- tibble::tibble(
    name = gsub(" .*", "", group),
    progress = gsub(".* ", "", group),
    branches = as.integer(table)
  )
  levels <- c("started", "built", "errored", "canceled")
  bins <- map(levels, ~tar_progress_branches_bin(.x, long))
  out <- progress[progress$type == "pattern",, drop = FALSE] # nolint
  out <- tibble::tibble(name = out$name, branches = out$branches)
  for (bin in bins) {
    out <- base::merge(x = out, y = bin, by = "name", all.x = TRUE)
  }
  for (level in levels) {
    out[[level]] <- replace_na(out[[level]], 0L)
  }
  tibble::as_tibble(out)
}

tar_progress_branches_bin <- function(level, long) {
  out <- long[long$progress %in% level,, drop = FALSE] # nolint
  out[[level]] <- out[["branches"]]
  out[["branches"]] <- NULL
  out[["progress"]] <- NULL
  out
}

# Just for the tar_watch() app. # nolint
tar_progress_branches_gt <- function() {
  progress <- tar_progress_branches(names = NULL, fields = NULL)
  out <- gt_borderless(progress)
  out <- gt::tab_style(
    out,
    gt::cell_text(weight = "bold"),
    locations = gt::cells_column_labels(everything())
  )
  colors <- data_frame(
    progress = c("started", "built", "errored", "canceled"),
    fill = c("#DC863B", "#E1BD6D", "#C93312", "#FAD510"),
    color = c("black", "black", "white", "black")
  )
  for (index in seq_len(nrow(colors))) {
    out <- gt::tab_style(
      out,
      style = list(
        gt::cell_fill(color = colors$fill[index]),
        gt::cell_text(color = colors$color[index])
      ),
      locations = gt::cells_body(
        columns = colors$progress[index],
        rows = progress[[colors$progress[index]]] > 0L
      )
    )
  }
  out
}

gt_borderless <- function(x) {
  out <- gt::gt(x)
  out <- gt::cols_width(out, everything() ~ gt::pct(16.6))
  out <- gt::cols_align(out, align = "left", columns = everything())
  out <- gt::tab_options(
    out,
    row.striping.include_table_body = TRUE,
    table.border.top.style = "hidden",
    table.border.bottom.style = "hidden",
    table_body.border.bottom.style = "hidden"
  )
  out <- gt::tab_style(
    out,
    style = gt::cell_borders(weight = gt::px(0)),
    locations = gt::cells_body()
  )
  out
}
