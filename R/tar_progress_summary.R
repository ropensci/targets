#' @title Summarize target progress.
#' @export
#' @family progress
#' @description Summarize the progress of a run of the pipeline.
#' @return A data frame with one row and the following
#'   optional columns that can be selected with `fields`.
#'   (`time` is omitted by default.)
#'   * `started`: number of targets that started and did not (yet) finish.
#'   * `built`: number of targets that completed without error or cancellation.
#'   * `errored`: number of targets that threw an error.
#'   * `canceled`: number of canceled targets (see [tar_cancel()]).
#'   * `since`: how long ago progress last changed (`Sys.time() - time`).
#'   * `time`: the time when the progress last changed
#'     (modification timestamp of the `_targets/meta/progress` file).
#' @inheritParams tar_validate
#' @param fields Optional, names of progress data columns to read.
#'   Set to `NULL` to read all fields.
#' @examples
#' if (identical(Sys.getenv("TAR_EXAMPLES"), "true")) { # for CRAN
#' tar_dir({ # tar_dir() runs code from a temp dir for CRAN.
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
  fields = c("skipped", "started", "built", "errored", "canceled", "since"),
  store = targets::tar_config_get("store")
) {
  tar_assert_scalar(store)
  tar_assert_chr(store)
  tar_assert_nzchar(store)
  time <- file.mtime(path_progress(path_store = store))
  progress <- progress_init(path_store = store)
  progress <- tibble::as_tibble(progress$database$read_condensed_data())
  progress <- progress[progress$type != "pattern",, drop = FALSE] # nolint
  out <- tibble::tibble(
    skipped = sum(progress$progress == "skipped"),
    started = sum(progress$progress == "started"),
    built = sum(progress$progress == "built"),
    errored = sum(progress$progress == "errored"),
    canceled = sum(progress$progress == "canceled"),
    since = units_seconds(difftime(Sys.time(), time, units = "secs")),
    time = time_stamp(time)
  )
  fields_quosure <- rlang::enquo(fields)
  fields <- tar_tidyselect_eval(fields_quosure, colnames(out)) %|||%
    colnames(out)
  out[, fields, drop = FALSE]
}

# Just for the tar_watch() app. # nolint
tar_progress_summary_gt <- function(path_store) {
  progress <- tar_progress_summary(fields = NULL, store = path_store)
  tar_progress_display_gt(progress)
}

tar_progress_display_gt <- function(progress) {
  out <- gt_borderless(progress)
  out <- gt::tab_style(
    out,
    gt::cell_text(weight = "bold"),
    locations = gt::cells_column_labels(everything())
  )
  colors <- data_frame(
    progress = c("skipped", "started", "built", "canceled", "errored"),
    fill = c("#3e236e", "#DC863B", "#E1BD6D", "#FAD510", "#C93312"),
    color = c("white", "black", "black", "black", "white")
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
