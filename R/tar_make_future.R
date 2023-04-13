#' @title Run a pipeline of targets in parallel with transient
#'   `future` workers.
#' @export
#' @family pipeline
#' @description This function is like [tar_make()] except that targets
#'   run in parallel with transient `future` workers. It requires
#'   that you declare your `future::plan()` inside the
#'   target script file (default: `_targets.R`).
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
#' if (identical(Sys.getenv("TAR_EXAMPLES"), "true")) { # for CRAN
#' tar_dir({ # tar_dir() runs code from a temp dir for CRAN.
#' tar_script({
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
  workers = targets::tar_config_get("workers"),
  callr_function = callr::r,
  callr_arguments = targets::tar_callr_args_default(callr_function, reporter),
  envir = parent.frame(),
  script = targets::tar_config_get("script"),
  store = targets::tar_config_get("store")
) {
  force(envir)
  tar_assert_package("future")
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
    workers = workers
  )$run()
  invisible()
}
