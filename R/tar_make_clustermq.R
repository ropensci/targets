#' @title Run a pipeline of targets in parallel with persistent
#'   `clustermq` workers.
#' @export
#' @family pipeline
#' @description This function is like [tar_make()] except that targets
#'   run in parallel with persistent `clustermq` workers. It requires
#'   that you set global options like `clustermq.scheduler` and
#'   `clustermq.template` inside the target script file
#'   (default: `_targets.R`).
#'   `clustermq` is not a strict dependency of `targets`,
#'   so you must install `clustermq` yourself.
#' @details To use with a cluster, you will need to set the global options
#'   `clustermq.scheduler` and `clustermq.template` inside the
#'   target script file (default: `_targets.R`).
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
#' if (identical(Sys.getenv("TAR_EXAMPLES"), "true")) {
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
  shortcut = targets::tar_config_get("shortcut"),
  reporter = targets::tar_config_get("reporter_make"),
  workers = targets::tar_config_get("workers"),
  log_worker = FALSE,
  callr_function = callr::r,
  callr_arguments = targets::callr_args_default(callr_function, reporter),
  envir = parent.frame(),
  script = targets::tar_config_get("script"),
  store = targets::tar_config_get("store")
) {
  force(envir)
  tar_assert_package("clustermq")
  tar_assert_scalar(shortcut)
  tar_assert_lgl(shortcut)
  tar_assert_flag(reporter, tar_reporters_make())
  tar_assert_scalar(workers)
  tar_assert_dbl(workers)
  tar_assert_ge(workers, 1)
  tar_assert_callr_function(callr_function)
  tar_assert_list(callr_arguments)
  targets_arguments <- list(
    path_store = store,
    names_quosure = rlang::enquo(names),
    shortcut = shortcut,
    reporter = reporter,
    workers = workers,
    log_worker = log_worker
  )
  out <- callr_outer(
    targets_function = tar_make_clustermq_inner,
    targets_arguments = targets_arguments,
    callr_function = callr_function,
    callr_arguments = callr_arguments,
    envir = envir,
    script = script,
    store = store,
    fun = "tar_make_clustermq"
  )
  invisible(out)
}

tar_make_clustermq_inner <- function(
  pipeline,
  path_store,
  names_quosure,
  shortcut,
  reporter,
  workers,
  log_worker
) {
  names <- tar_tidyselect_eval(names_quosure, pipeline_get_names(pipeline))
  clustermq_init(
    pipeline = pipeline,
    meta = meta_init(path_store = path_store),
    names = names,
    shortcut = shortcut,
    queue = "parallel",
    reporter = reporter,
    envir = tar_option_get("envir"),
    workers = workers,
    log_worker = log_worker
  )$run()
  invisible()
}
