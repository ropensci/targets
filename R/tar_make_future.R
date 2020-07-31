#' @title Run a pipeline of targets in parallel with transient
#'   `future` workers.
#' @export
#' @description This function is like [tar_make()] except that targets
#'   run in parallel with transient `future` workers. It requires
#'   that you declare your `future::plan()` inside the `_targets.R` script.
#'   `future` is not a strict dependency of `targets`,
#'   so you must install `future` yourself.
#' @details To configure `tar_make_future()` with a computing cluster,
#'   see the `future.batchtools` package documentation.
#' @return `NULL` except if `callr_function = callr::r_bg()`, in which case
#'   a handle to the `callr` background process is returned. Either way,
#'   the value is invisibly returned.
#' @inheritParams tar_make
#' @param workers Positive integer, maximum number of transient
#'   `future` workers allowed to run at any given time.
#' @examples
#' \dontrun{
#' tar_dir({
#' tar_script({
#'   future::plan(future::multisession)
#'   tar_option_set()
#'   tar_pipeline(tar_target(x, 1 + 1))
#' })
#' tar_make_future()
#' })
#' }
tar_make_future <- function(
  names = NULL,
  reporter = "verbose",
  garbage_collection = FALSE,
  workers = 1L,
  callr_function = callr::r,
  callr_arguments = list()
) {
  assert_target_script()
  reporter <- match.arg(reporter, choices = tar_make_reporters())
  assert_lgl(garbage_collection, "garbage_collection must be logical.")
  assert_callr_function(callr_function)
  assert_list(callr_arguments, "callr_arguments mut be a list.")
  targets_arguments <- list(
    names_quosure = rlang::enquo(names),
    reporter = reporter,
    garbage_collection = garbage_collection,
    workers = workers
  )
  out <- callr_outer(
    targets_function = tar_make_future_inner,
    targets_arguments = targets_arguments,
    callr_function = callr_function,
    callr_arguments = callr_arguments
  )
  invisible(out)
}

tar_make_future_inner <- function(
  pipeline,
  names_quosure,
  reporter,
  garbage_collection,
  workers
) {
  pipeline_validate(pipeline)
  names <- tar_tidyselect(names_quosure, pipeline_get_names(pipeline))
  algorithm_init(
    subclass = "future",
    pipeline = pipeline,
    names = names,
    queue = "parallel",
    reporter = reporter,
    garbage_collection = garbage_collection,
    workers = workers
  )$run()
  invisible()
}
