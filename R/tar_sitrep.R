#' @title Show the cue-by-cue status of each target.
#' @export
#' @description For each target, report which cues are activated.
#'   Except for the `never` cue, the target will rerun in [tar_make()]
#'   if any cue is activated. The target is suppressed if the `never`
#'   cue is `TRUE`. See [tar_cue()] for details.
#' @details Caveats:
#'   * [tar_cue()] allows you to change/suppress cues, so the return
#'     value will depend on the settings you supply to [tar_cue()].
#'   * If a pattern tries to branches over a target that does not exist
#'     in storage, then the branches are omitted from the output.
#'   * `tar_sitrep()` is myopic. It only considers what happens to the
#'     immediate target and its immediate upstream dependencies,
#'     and it makes no attempt to propagate invalidation downstream.
#' @return A data frame with one row per target/object and one column
#'   per cue. Each element is a logical to indicate whether the cue
#'   is activated for the target.
#'   See the `field` argument in this help file for details.
#' @inheritParams tar_manifest
#' @inheritParams tar_outdated
#' @param names Optional, names of the targets. If supplied, `tar_sitrep()`
#'   only returns metadata on these targets.
#'   You can supply symbols, a character vector,
#'   or `tidyselect` helpers like [starts_with()].
#' @param fields Optional, names of columns/fields to select. If supplied,
#'   `tar_sitrep()` only returns the selected metadata columns.
#'   You can supply symbols, a character vector, or `tidyselect` helpers
#'   like [starts_with()]. The `name` column is always included first
#'   no matter what you select. Choices:
#'   * `name`: name of the target or global object.
#'   * `record`: Whether the `record` cue is activated:
#'     `TRUE` if the target is not in the metadata ([tar_meta()]),
#'     or if the target errored during the last [tar_make()],
#'     or if the class of the target changed.
#'   * `always`: Whether `mode` in [tar_cue()] is `"always"`.
#'     If `TRUE`, [tar_make()] always runs the target.
#'   * `never`: Whether `mode` in [tar_cue()] is `"never"`.
#'     If `TRUE`, [tar_make()] will only run if the
#'     `record` cue activates.
#'   * `command`: Whether the target's command changed since last time.
#'     Always `TRUE` if the `record` cue is activated.
#'     Otherwise, always `FALSE` if the `command` cue is suppressed.
#'   * `depend`: Whether the data/output of at least one of the target's
#'     dependencies changed since last time.
#'     Dependencies are targets, functions,
#'     and global objects directly upstream.
#'     Call `tar_outdated(targets_only = FALSE)` or
#'     `tar_visnetwork(targets_only = FALSE)` to see exactly which
#'     dependencies are outdated.
#'     Always `NA` if the `record` cue is activated.
#'     Otherwise, always `FALSE` if the `depend` cue is suppressed.
#'   * `format`: Whether the storage format of the target
#'     is different from last time.
#'     Always `NA` if the `record` cue is activated.
#'     Otherwise, always `FALSE` if the `format` cue is suppressed.
#'   * `iteration`: Whether the iteration mode of the target
#'     is different from last time.
#'     Always `NA` if the `record` cue is activated.
#'     Otherwise, always `FALSE` if the `iteration` cue is suppressed.
#'   * `file`: Whether the file(s) with the target's return value
#'     are missing or different from last time.
#'     Always `NA` if the `record` cue is activated.
#'     Otherwise, always `FALSE` if the `file` cue is suppressed.
#' @examples
#' if (identical(Sys.getenv("TAR_LONG_EXAMPLES"), "true")) {
#' tar_dir({ # tar_dir() runs code from a temporary directory.
#' tar_script({
#'   list(
#'     tar_target(x, seq_len(2)),
#'     tar_target(y, 2 * x, pattern = map(x))
#'   )
#' }, ask = FALSE)
#' tar_make()
#' tar_sitrep()
#' tar_meta(starts_with("y_"))
#' })
#' }
tar_sitrep <- function(
  names = NULL,
  fields = NULL,
  reporter = "silent",
  callr_function = callr::r,
  callr_arguments = list(spinner = identical(reporter, "silent"))
) {
  assert_script()
  names_quosure <- rlang::enquo(names)
  fields_quosure <- rlang::enquo(fields)
  assert_scalar(reporter, "reporter arg of tar_outdated() must have length 1.")
  assert_in(
    reporter,
    c("forecast", "silent"),
    "reporter arg of tar_outdated() must either be \"silent\" or \"forecast\""
  )
  assert_callr_function(callr_function)
  assert_list(callr_arguments, "callr_arguments mut be a list.")
  targets_arguments <- list(
    names_quosure = rlang::enquo(names),
    fields_quosure = rlang::enquo(fields),
    reporter = reporter
  )
  callr_outer(
    targets_function = tar_sitrep_inner,
    targets_arguments = targets_arguments,
    callr_function = callr_function,
    callr_arguments = callr_arguments
  )
}

tar_sitrep_inner <- function(
  pipeline,
  names_quosure,
  fields_quosure,
  reporter
) {
  names_all <- pipeline_get_names(pipeline)
  names <- eval_tidyselect(names_quosure, names_all)
  meta <- meta_init()
  sitrep <- sitrep_init(
    pipeline = pipeline,
    names = names,
    queue = "sequential",
    reporter = reporter
  )
  sitrep$run()
  out <- tibble::as_tibble(data.table::rbindlist(as.list(sitrep$sitrep)))
  if (!is.null(names)) {
    out <- out[match(names, out$name),, drop = FALSE] # nolint
  }
  fields <- eval_tidyselect(fields_quosure, colnames(out)) %||% colnames(out)
  out[, base::union("name", fields), drop = FALSE]
}
