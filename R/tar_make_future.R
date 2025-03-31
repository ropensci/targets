#' @title Superseded. Run a pipeline of targets in parallel with transient
#'   `future` workers.
#' @export
#' @family pipeline
#' @description Superseded. Use [tar_make()] with `crew`:
#'   <https://books.ropensci.org/targets/crew.html>.
#' @details This function is like [tar_make()] except that targets
#'   run in parallel with transient `future` workers. It requires
#'   that you declare your `future::plan()` inside the
#'   target script file (default: `_targets.R`).
#'   `future` is not a strict dependency of `targets`,
#'   so you must install `future` yourself.
#'
#'   To configure `tar_make_future()` with a computing cluster,
#'   see the `future.batchtools` package documentation.
#' @inheritSection tar_meta Storage access
#' @return `NULL` except if `callr_function = callr::r_bg()`, in which case
#'   a handle to the `callr` background process is returned. Either way,
#'   the value is invisibly returned.
#' @inheritParams tar_make
#' @param workers Positive integer, maximum number of transient
#'   `future` workers allowed to run at any given time.
#' @examples
#' if (identical(Sys.getenv("TAR_EXAMPLES"), "true")) { # for CRAN
#' tar_dir({ # tar_dir() runs code from a temp dir for CRAN.
#' tar_script({
#'   library(targets)
#'   library(tarchetypes)
#'   future::plan(future::multisession, workers = 2)
#'   list(
#'     tar_target(x, 1 + 1),
#'     tar_target(y, 1 + 1)
#'   )
#' }, ask = FALSE)
#' tar_make_future()
#' })
#' }
tar_make_future <- function(
  names = NULL,
  shortcut = targets::tar_config_get("shortcut"),
  reporter = targets::tar_config_get("reporter_make"),
  seconds_meta_append = targets::tar_config_get("seconds_meta_append"),
  seconds_meta_upload = targets::tar_config_get("seconds_meta_upload"),
  seconds_reporter = targets::tar_config_get("seconds_reporter"),
  seconds_interval = targets::tar_config_get("seconds_interval"),
  workers = targets::tar_config_get("workers"),
  callr_function = callr::r,
  callr_arguments = targets::tar_callr_args_default(callr_function, reporter),
  envir = parent.frame(),
  script = targets::tar_config_get("script"),
  store = targets::tar_config_get("store"),
  garbage_collection = NULL
) {
  tar_assert_allow_meta("tar_make_future", store)
  force(envir)
  tar_assert_package("future")
  tar_assert_scalar(shortcut)
  tar_assert_lgl(shortcut)
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
  tar_deprecate_seconds_interval(seconds_interval)
  reporter <- tar_make_reporter(reporter)
  if_any(
    is.null(garbage_collection),
    NULL,
    tar_warn_deprecate(
      "The garbage_collection argument of tar_make_future() was deprecated ",
      "in targets version 1.8.0.9004 (2024-10-22). The garbage_collection ",
      "argument of tar_option_set() is more unified and featureful now. ",
      "Please have a look at its documentation."
    )
  )
  targets_arguments <- list(
    path_store = store,
    names_quosure = rlang::enquo(names),
    shortcut = shortcut,
    reporter = reporter,
    seconds_meta_append = seconds_meta_append,
    seconds_meta_upload = seconds_meta_upload,
    workers = workers
  )
  out <- callr_outer(
    targets_function = tar_make_future_inner,
    targets_arguments = targets_arguments,
    callr_function = callr_function,
    callr_arguments = callr_arguments,
    envir = envir,
    script = script,
    store = store,
    fun = "tar_make_future"
  )
  invisible(out)
}

tar_make_future_inner <- function(
  pipeline,
  path_store,
  names_quosure,
  shortcut,
  reporter,
  seconds_meta_append,
  seconds_meta_upload,
  workers
) {
  names <- tar_tidyselect_eval(names_quosure, pipeline_get_names(pipeline))
  future_init(
    pipeline = pipeline,
    meta_init(path_store = path_store),
    names = names,
    shortcut = shortcut,
    queue = "parallel",
    reporter = reporter,
    seconds_meta_append = seconds_meta_append,
    seconds_meta_upload = seconds_meta_upload,
    workers = workers
  )$run()
  invisible()
}
