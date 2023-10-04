#' @title Superseded. Run a pipeline with persistent `clustermq` workers.
#' @export
#' @family pipeline
#' @description Superseded. Use [tar_make()] with `crew`:
#'   <https://books.ropensci.org/targets/crew.html>.
#' @details `tar_make_clustermq()` is like [tar_make()] except that targets
#'   run in parallel on persistent workers. A persistent worker is an
#'   R process that runs for a long time and builds multiple
#'   targets during its lifecycle. Persistent
#'   workers launch as soon as the pipeline reaches an outdated
#'   target with `deployment = "worker"`, and they keep running
#'   until the pipeline starts to wind down.
#'
#'   To configure `tar_make_clustermq()`, you must configure
#'   the `clustermq` package. To do this, set global options
#'   `clustermq.scheduler` and `clustermq.template`
#'   inside the target script file (default: `_targets.R`).
#'   To read more about configuring `clustermq` for your scheduler, visit
#'   <https://mschubert.github.io/clustermq/articles/userguide.html#configuration> # nolint
#'   or <https://books.ropensci.org/targets/hpc.html>.
#'   `clustermq` is not a strict dependency of `targets`,
#'   so you must install `clustermq` yourself.
#' @inheritSection tar_meta Storage access
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
#' if (identical(Sys.getenv("TAR_EXAMPLES"), "true")) { # for CRAN
#' tar_dir({ # tar_dir() runs code from a temp dir for CRAN.
#' tar_script({
#'   options(clustermq.scheduler = "multiprocess") # Does not work on Windows.
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
  seconds_meta_append = targets::tar_config_get("seconds_meta_append"),
  seconds_meta_upload = targets::tar_config_get("seconds_meta_upload"),
  seconds_reporter = targets::tar_config_get("seconds_reporter"),
  seconds_interval = targets::tar_config_get("seconds_interval"),
  workers = targets::tar_config_get("workers"),
  log_worker = FALSE,
  callr_function = callr::r,
  callr_arguments = targets::tar_callr_args_default(callr_function, reporter),
  envir = parent.frame(),
  script = targets::tar_config_get("script"),
  store = targets::tar_config_get("store"),
  garbage_collection = targets::tar_config_get("garbage_collection")
) {
  # Need to suppress tests on covr only, due to
  # https://github.com/r-lib/covr/issues/315.
  # Cannot use multicore clustermq backend
  # due to https://github.com/ropensci/targets/discussions/780
  # nocov start
  tar_assert_allow_meta("tar_make_clustermq", store)
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
  tar_assert_dbl(seconds_meta_append)
  tar_assert_scalar(seconds_meta_append)
  tar_assert_none_na(seconds_meta_append)
  tar_assert_ge(seconds_meta_append, 0)
  tar_assert_dbl(seconds_meta_upload)
  tar_assert_scalar(seconds_meta_upload)
  tar_assert_none_na(seconds_meta_upload)
  tar_assert_ge(seconds_meta_upload, 0)
  tar_assert_dbl(seconds_reporter)
  tar_assert_scalar(seconds_reporter)
  tar_assert_none_na(seconds_reporter)
  tar_assert_ge(seconds_reporter, 0)
  tar_deprecate_seconds_interval(seconds_interval)
  tar_assert_lgl(garbage_collection)
  tar_assert_scalar(garbage_collection)
  tar_assert_none_na(garbage_collection)
  targets_arguments <- list(
    path_store = store,
    names_quosure = rlang::enquo(names),
    shortcut = shortcut,
    reporter = reporter,
    seconds_meta_append = seconds_meta_append,
    seconds_meta_upload = seconds_meta_upload,
    seconds_reporter = seconds_reporter,
    garbage_collection = garbage_collection,
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
  seconds_meta_append,
  seconds_meta_upload,
  seconds_reporter,
  garbage_collection,
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
    seconds_meta_append = seconds_meta_append,
    seconds_meta_upload = seconds_meta_upload,
    seconds_reporter = seconds_reporter,
    garbage_collection = garbage_collection,
    envir = tar_option_get("envir"),
    workers = workers,
    log_worker = log_worker
  )$run()
  invisible()
}
# nocov end
