#' @title Read the target progress of the latest run of the pipeline.
#' @export
#' @description Read a project's target progress data for the most recent
#'   run of [tar_make()] or similar. Only the most recent record is shown.
#' @return A data frame with one row per target per progress status
#'   and the following columns.
#'   * `name`: name of the pattern.
#'   * `progress`: progress status: `"running"`, `"built"`, `"cancelled"`,
#'     or `"errored"`.
#'   * `branches`: number of branches in the progress category.
#'   * `total`: total number of branches planned for the whole pattern.
#'     Values within the same pattern should all be equal.
#' @param names Optional, names of the targets. If supplied, `tar_progress()`
#'   only returns progress information on these targets.
#'   You can supply symbols, a character vector,
#'   or `tidyselect` helpers like [starts_with()].
#' @param fields Optional, names of progress data columns to read.
#'   Set to `NULL` to read all fields.
#' @examples
#' if (identical(Sys.getenv("TAR_LONG_EXAMPLES"), "true")) {
#' tar_dir({ # tar_dir() runs code from a temporary directory.
#' tar_script({
#'   list(
#'     tar_target(x, seq_len(2)),
#'     tar_target(y, x, pattern = map(x)),
#'     tar_target(z, stopifnot(y < 1.5), pattern = map(y))
#'   )
#' }, ask = FALSE)
#' try(tar_make())
#' tar_progress_branches()
#' })
#' }
tar_progress_branches <- function(names = NULL, fields = NULL) {
  assert_store()
  assert_path(file.path("_targets/meta/progress"))
  out <- tibble::as_tibble(progress_init()$database$read_condensed_data())
  out <- tar_progress_branches_summary(out)
  names_quosure <- rlang::enquo(names)
  fields_quosure <- rlang::enquo(fields)
  names <- eval_tidyselect(names_quosure, out$name)
  fields <- eval_tidyselect(fields_quosure, colnames(out)) %||% colnames(out)
  if (!is.null(names)) {
    out <- out[match(names, out$name),, drop = FALSE] # nolint
  }
  out <- out[, base::union("name", fields), drop = FALSE]
  out[order(out$name),, drop = FALSE] # nolint
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
  levels <- c("running", "built", "canceled", "errored")
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
    progress = c("running", "built", "canceled", "errored"),
    fill = c("#DC863B", "#E1BD6D", "#FAD510", "#C93312"),
    color = c("black", "black", "black", "white")
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
    gt::cell_borders(weight = gt::px(0)),
    locations = gt::cells_body()
  )
}
