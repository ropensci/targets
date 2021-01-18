#' @title Read the target progress of the latest run of the pipeline.
#' @export
#' @description Read a project's target progress data for the most recent
#'   run of [tar_make()] or similar. Only the most recent record is shown.
#' @return A data frame with one row per target and the following columns:
#'   * `name`: name of the target or global object.
#'   * `progress`: the most recent progress update of that target.
#'     Could be `"running"`, `"built"`, `"cancelled"`, or `"failed"`.
#' @param names Optional, names of the targets. If supplied, `tar_progress()`
#'   only returns progress information on these targets.
#'   You can supply symbols, a character vector,
#'   or `tidyselect` helpers like [starts_with()].
#' @examples
#' if (identical(Sys.getenv("TARGETS_LONG_EXAMPLES"), "true")) {
#' tar_dir({ # Write all files to a temporary directory.
#' tar_script(
#'   list(
#'     tar_target(x, seq_len(2)),
#'     tar_target(y, 2 * x, pattern = map(x))
#'   )
#' )
#' tar_make()
#' tar_progress()
#' tar_progress(starts_with("y_"))
#' })
#' }
tar_progress <- function(names = NULL) {
  assert_store()
  assert_path(file.path("_targets/meta/progress"))
  out <- tibble::as_tibble(progress_init()$database$read_condensed_data())
  names_quosure <- rlang::enquo(names)
  names <- eval_tidyselect(names_quosure, out$name)
  if (!is.null(names)) {
    out <- out[match(names, out$name),, drop = FALSE] # nolint
  }
  out
}
