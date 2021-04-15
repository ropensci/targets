#' @title Run a pipeline of targets.
#' @export
#' @family pipeline
#' @description Run the pipeline you defined in `_targets.R`. `tar_make()`
#'   runs the correct targets in the correct order and stores the return
#'   values in `_targets/objects/`.
#' @return `NULL` except if `callr_function = callr::r_bg()`, in which case
#'   a handle to the `callr` background process is returned. Either way,
#'   the value is invisibly returned.
#' @inheritParams tar_validate
#' @param names Names of the targets to build or check. Set to `NULL` to
#'   check/build all the targets (default). Otherwise, you can supply
#'   symbols, a character vector, or `tidyselect` helpers like [starts_with()].
#' @param reporter Character of length 1, name of the reporter to user.
#'   Controls how messages are printed as targets run in the pipeline.
#'   Defaults to the `TAR_MAKE_REPORTER` environment variable if set
#'   and `"verbose"` otherwise. Choices:
#'   * `"verbose"`: print one message for each target that runs (default).
#'   * `"silent"`: print nothing.
#'   * `"timestamp"`: print a time-stamped message for each target that runs.
#'   * `"summary"`: print a running total of the number of each targets in
#'     each status category (queued, started, skipped, build, canceled,
#'     or errored). Also show a timestamp (`"%H:%M %OS2"` `strptime()` format)
#'     of the last time the progress changed and printed to the screen.
#' @examples
#' tar_dir({ # tar_dir() runs code from a temporary directory.
#' tar_script({
#'   tar_option_set()
#'   list(tar_target(x, 1 + 1))
#' })
#' tar_make()
#' tar_script({
#'   tar_option_set()
#'   list(
#'     tar_target(y1, 1 + 1),
#'     tar_target(y2, 1 + 1),
#'     tar_target(z, y1 + y2)
#'   )
#' }, ask = FALSE)
#' tar_make(starts_with("y")) # Only builds y1 and y2.
#' })
tar_make <- function(
  names = NULL,
  reporter = Sys.getenv("TAR_MAKE_REPORTER", unset = "verbose"),
  callr_function = callr::r,
  callr_arguments = targets::callr_args_default(callr_function, reporter)
) {
  assert_script()
  assert_flag(reporter, tar_make_reporters())
  assert_callr_function(callr_function)
  assert_list(callr_arguments, "callr_arguments mut be a list.")
  targets_arguments <- list(
    names_quosure = rlang::enquo(names),
    reporter = reporter
  )
  out <- callr_outer(
    targets_function = tar_make_inner,
    targets_arguments = targets_arguments,
    callr_function = callr_function,
    callr_arguments = callr_arguments
  )
  invisible(out)
}

tar_make_inner <- function(pipeline, names_quosure, reporter) {
  pipeline_reset_deployments(pipeline)
  names <- eval_tidyselect(names_quosure, pipeline_get_names(pipeline))
  local_init(
    pipeline = pipeline,
    names = names,
    queue = "sequential",
    reporter = reporter
  )$run()
  invisible()
}

tar_make_reporters <- function() {
  c("verbose", "silent", "timestamp", "summary")
}
