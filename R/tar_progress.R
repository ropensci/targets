#' @title Read progress.
#' @export
#' @family progress
#' @description Read a project's target progress data for the most recent
#'   run of [tar_make()] or similar. Only the most recent record is shown.
#' @inheritSection tar_meta Storage access
#' @return A data frame with one row per target and the following columns:
#'   * `name`: name of the target.
#'   * `type`: type of target: `"stem"` for non-branching targets,
#'     `"pattern"` for dynamically branching targets, and `"branch"`
#'     for dynamic branches.
#'   * `parent`: name of the target's parent. For branches, this is the
#'     name of the associated pattern. For other targets, the pattern
#'     is just itself.
#'   * `branches`: number of dynamic branches of a pattern. 0 for non-patterns.
#'   * `progress`: the most recent progress update of that target.
#'     Could be `"dispatched"`, `"completed"`, "`skipped`", `"canceled"`,
#'       or `"errored"`. `"dispatched"` means the target was sent off
#'       to be run, but in the case of `tar_make()` with a `crew` controller,
#'       the target might not actually start running right away if the `crew`
#'       workers are all busy.
#' @inheritParams tar_validate
#' @param names Optional, names of the targets. If supplied, the
#'   output is restricted to the selected targets.
#'   The object supplied to `names` should be `NULL` or a
#'   `tidyselect` expression like [any_of()] or [starts_with()]
#'   from `tidyselect` itself, or [tar_described_as()] to select target names
#'   based on their descriptions.
#' @param fields Optional, names of progress data columns to read.
#'   Set to `NULL` to read all fields.
#' @examples
#' if (identical(Sys.getenv("TAR_EXAMPLES"), "true")) { # for CRAN
#' tar_dir({ # tar_dir() runs code from a temp dir for CRAN.
#' tar_script({
#'   library(targets)
#'   library(tarchetypes)
#'   list(
#'     tar_target(x, seq_len(2)),
#'     tar_target(y, 2 * x, pattern = map(x))
#'   )
#' }, ask = FALSE)
#' tar_make()
#' tar_progress()
#' tar_progress(starts_with("y_")) # see also any_of()
#' })
#' }
tar_progress <- function(
  names = NULL,
  fields = "progress",
  store = targets::tar_config_get("store")
) {
  tar_assert_allow_meta("tar_progress", store)
  tar_assert_scalar(store)
  tar_assert_chr(store)
  tar_assert_nzchar(store)
  progress <- progress_init(path_store = store)
  out <- tibble::as_tibble(progress$database$read_condensed_data())
  names_quosure <- rlang::enquo(names)
  fields_quosure <- rlang::enquo(fields)
  names <- tar_tidyselect_eval(names_quosure, out$name)
  fields <- tar_tidyselect_eval(fields_quosure, colnames(out)) %|||%
    colnames(out)
  if (!is.null(names)) {
    out <- out[match(names, out$name),, drop = FALSE] # nolint
  }
  out[, base::union("name", fields), drop = FALSE]
}
