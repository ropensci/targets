#' @title Show the cue-by-cue status of each target.
#' @export
#' @family inspect
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
#'   The object supplied to `names` should be `NULL` or a
#'   `tidyselect` expression like [any_of()] or [starts_with()]
#'   from `tidyselect` itself, or [tar_described_as()] to select target names
#'   based on their descriptions.
#' @param shortcut Logical of length 1, how to interpret the `names` argument.
#'   If `shortcut` is `FALSE` (default) then the function checks
#'   all targets upstream of `names` as far back as the dependency graph goes.
#'   If `TRUE`, then the function only checks the targets in `names`
#'   and uses stored metadata for information about upstream dependencies
#'   as needed. `shortcut = TRUE` increases speed if there are a lot of
#'   up-to-date targets, but it assumes all the dependencies
#'   are up to date, so please use with caution.
#'   Use with caution. `shortcut = TRUE` only works if you set `names`.
#' @param fields Optional, names of columns/fields to select. If supplied,
#'   `tar_sitrep()` only returns the selected metadata columns.
#'   You can supply symbols or `tidyselect` helpers
#'   like [any_of()] and [starts_with()].
#'   The `name` column is always included first
#'   no matter what you select. Choices:
#'   * `name`: name of the target or global object.
#'   * `meta`: Whether the `meta` cue is activated:
#'     `TRUE` if the target is not in the metadata ([tar_meta()]),
#'     or if the target errored during the last [tar_make()],
#'     or if the class of the target changed.
#'   * `always`: Whether `mode` in [tar_cue()] is `"always"`.
#'     If `TRUE`, [tar_make()] always runs the target.
#'   * `never`: Whether `mode` in [tar_cue()] is `"never"`.
#'     If `TRUE`, [tar_make()] will only run if the
#'     `meta` cue activates.
#'   * `command`: Whether the target's command changed since last time.
#'     Always `TRUE` if the `meta` cue is activated.
#'     Otherwise, always `FALSE` if the `command` cue is suppressed.
#'   * `depend`: Whether the data/output of at least one of the target's
#'     dependencies changed since last time.
#'     Dependencies are targets, functions,
#'     and global objects directly upstream.
#'     Call `tar_outdated(targets_only = FALSE)` or
#'     `tar_visnetwork(targets_only = FALSE)` to see exactly which
#'     dependencies are outdated.
#'     Always `NA` if the `meta` cue is activated.
#'     Otherwise, always `FALSE` if the `depend` cue is suppressed.
#'   * `format`: Whether the storage format of the target
#'     is different from last time.
#'     Always `NA` if the `meta` cue is activated.
#'     Otherwise, always `FALSE` if the `format` cue is suppressed.
#'   * `repository`: Whether the storage repository of the target
#'     is different from last time.
#'     Always `NA` if the `meta` cue is activated.
#'     Otherwise, always `FALSE` if the `format` cue is suppressed.
#'   * `iteration`: Whether the iteration mode of the target
#'     is different from last time.
#'     Always `NA` if the `meta` cue is activated.
#'     Otherwise, always `FALSE` if the `iteration` cue is suppressed.
#'   * `file`: Whether the file(s) with the target's return value
#'     are missing or different from last time.
#'     Always `NA` if the `meta` cue is activated.
#'     Otherwise, always `FALSE` if the `file` cue is suppressed.
#' @examples
#' if (identical(Sys.getenv("TAR_EXAMPLES"), "true")) { # for CRAN
#' tar_dir({ # tar_dir() runs code from a temp dir for CRAN.
#' tar_script({
#'   list(
#'     tar_target(x, seq_len(2)),
#'     tar_target(y, 2 * x, pattern = map(x))
#'   )
#' }, ask = FALSE)
#' tar_make()
#' tar_sitrep()
#' tar_meta(starts_with("y_")) # see also any_of()
#' })
#' }
tar_sitrep <- function(
  names = NULL,
  fields = NULL,
  shortcut = targets::tar_config_get("shortcut"),
  reporter = targets::tar_config_get("reporter_outdated"),
  seconds_reporter = targets::tar_config_get("seconds_reporter_outdated"),
  callr_function = callr::r,
  callr_arguments = targets::tar_callr_args_default(callr_function, reporter),
  envir = parent.frame(),
  script = targets::tar_config_get("script"),
  store = targets::tar_config_get("store")
) {
  tar_assert_allow_meta("tar_sitrep", store)
  force(envir)
  names_quosure <- rlang::enquo(names)
  fields_quosure <- rlang::enquo(fields)
  tar_assert_scalar(shortcut)
  tar_assert_lgl(shortcut)
  tar_assert_flag(reporter, tar_reporters_outdated())
  reporter <- tar_outdated_reporter(reporter)
  tar_assert_callr_function(callr_function)
  tar_assert_list(callr_arguments)
  tar_message_meta(store = store)
  targets_arguments <- list(
    path_store = store,
    names_quosure = rlang::enquo(names),
    shortcut = shortcut,
    fields_quosure = rlang::enquo(fields),
    reporter = reporter
  )
  callr_outer(
    targets_function = tar_sitrep_inner,
    targets_arguments = targets_arguments,
    callr_function = callr_function,
    callr_arguments = callr_arguments,
    envir = envir,
    script = script,
    store = store,
    fun = "tar_sitrep"
  )
}

tar_sitrep_inner <- function(
  pipeline,
  path_store,
  names_quosure,
  shortcut,
  fields_quosure,
  reporter
) {
  names_all <- pipeline_get_names(pipeline)
  names <- tar_tidyselect_eval(names_quosure, names_all)
  sitrep <- sitrep_init(
    pipeline = pipeline,
    meta = meta_init(path_store = path_store),
    names = names,
    shortcut = shortcut,
    queue = "sequential",
    reporter = reporter
  )
  sitrep$run()
  out <- tibble::as_tibble(data.table::rbindlist(as.list(sitrep$sitrep)))
  if (!is.null(names)) {
    out <- out[match(names, out$name),, drop = FALSE] # nolint
  }
  fields <- tar_tidyselect_eval(fields_quosure, colnames(out)) %|||%
    colnames(out)
  out[, base::union("name", fields), drop = FALSE]
}
