#' @title Run a pipeline of targets in parallel with persistent
#'   `clustermq` workers.
#' @export
#' @family pipeline
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
#'   Wildcards in the template file are filled in with elements from
#'   `tar_option_get("resources")`.
#' @return `NULL` except if `callr_function = callr::r_bg()`, in which case
#'   a handle to the `callr` background process is returned. Either way,
#'   the value is invisibly returned.
#' @inheritParams tar_make_future
#' @param workers Positive integer, number of persistent `clustermq` workers
#'   to create.
#' @param log_worker Logical, whether to write a log file for each worker.
#'   Same as the `log_worker` argument of `clustermq::Q()`
#'   and `clustermq::workers()`.
#' @examples
#' if (!identical(tolower(Sys.info()[["sysname"]]), "windows")) {
#' if (identical(Sys.getenv("TAR_LONG_EXAMPLES"), "true")) {
#' tar_dir({ # tar_dir() runs code from a temporary directory.
#' tar_script({
#'   options(clustermq.scheduler = "multicore") # Does not work on Windows.
#'   tar_option_set()
#'   list(tar_target(x, 1 + 1))
#' }, ask = FALSE)
#' tar_make_clustermq()
#' })
#' }
#' }
tar_make_clustermq <- function(
  names = NULL,
  reporter = Sys.getenv("TAR_MAKE_REPORTER", unset = "verbose"),
  workers = 1L,
  log_worker = FALSE,
  callr_function = callr::r,
  callr_arguments = targets::callr_args_default(callr_function, reporter)
) {
  assert_package("clustermq")
  assert_script()
  assert_flag(reporter, tar_make_reporters())
  assert_callr_function(callr_function)
  assert_list(callr_arguments, "callr_arguments mut be a list.")
  targets_arguments <- list(
    names_quosure = rlang::enquo(names),
    reporter = reporter,
    workers = workers,
    log_worker = log_worker
  )
  out <- callr_outer(
    targets_function = tar_make_clustermq_inner,
    targets_arguments = targets_arguments,
    callr_function = callr_function,
    callr_arguments = callr_arguments
  )
  invisible(out)
}

tar_make_clustermq_inner <- function(
  pipeline,
  names_quosure,
  reporter,
  workers,
  log_worker
) {
  names <- eval_tidyselect(names_quosure, pipeline_get_names(pipeline))
  clustermq_init(
    pipeline = pipeline,
    names = names,
    queue = "parallel",
    reporter = reporter,
    workers = workers,
    log_worker = log_worker
  )$run()
  invisible()
}
