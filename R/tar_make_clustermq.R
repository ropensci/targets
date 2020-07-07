#' @title Run a pipeline of targets in parallel with persistent
#'   `clustermq` workers.
#' @export
#' @description This function is like [tar_make()] except that targets
#'   run in parallel with persistent `clustermq` workers. It requires
#'   that you set global options like `clustermq.scheduler` and
#'   `clustermq.template` inside the `_targets.R` script.
#'   `clustermq` is not a strict dependency of `targets`,
#'   so you must install `clustermq` yourself.
#' @details To use with a cluster, you will need to set the global options
#'   `clustermq.scheduler` and `clustermq.template` inside `_targets.R`.
#'   To read more about configuring `clustermq` for your scheduler, visit
#'   <https://mschubert.github.io/clustermq/articles/userguide.html#configuration> # nolint
#'   and navigate to the appropriate link under "Setting up the scheduler".
#' @return Nothing.
#' @inheritParams tar_make_future
#' @param template Named list of values to insert as fields
#'   in the `clustermq` template file, such as computing resource requirements.
#' @examples
#' \dontrun{
#' tar_dir({
#' tar_script({
#'   options(clustermq.scheduler = "multicore")
#'   tar_options()
#'   tar_pipeline(tar_target(x, 1 + 1))
#' })
#' tar_make_clustermq()
#' })
#' }
tar_make_clustermq <- function(
  names = NULL,
  reporter = "verbose",
  garbage_collection = FALSE,
  workers = 1L,
  template = list(),
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
    workers = workers,
    template = template
  )
  callr_outer(
    targets_function = tar_make_clustermq_inner,
    targets_arguments = targets_arguments,
    callr_function = callr_function,
    callr_arguments = callr_arguments
  )
  invisible()
}

tar_make_clustermq_inner <- function(
  pipeline,
  names_quosure,
  reporter,
  garbage_collection,
  workers,
  template
) {
  names <- tar_tidyselect(names_quosure, pipeline_get_names(pipeline))
  algorithm_init(
    subclass = "clustermq",
    pipeline = pipeline,
    names = names,
    reporter = reporter,
    garbage_collection = garbage_collection,
    workers = workers,
    template = template
  )$run()
}
