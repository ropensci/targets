#' @title Run a pipeline of targets.
#' @export
#' @family pipeline
#' @description Run the pipeline you defined in the targets
#'   script file (default: `_targets.R`). `tar_make()`
#'   runs the correct targets in the correct order and stores the return
#'   values in `_targets/objects/`. Use [tar_read()] to read a target
#'   back into R, and see
#'   <https://docs.ropensci.org/targets/reference/index.html#clean>
#'   to manage output files.
#' @inheritSection tar_meta Storage access
#' @return `NULL` except if `callr_function = callr::r_bg()`, in which case
#'   a handle to the `callr` background process is returned. Either way,
#'   the value is invisibly returned.
#' @inheritParams tar_validate
#' @param names Names of the targets to run or check. Set to `NULL` to
#'   check/run all the targets (default).
#'   The object supplied to `names` should be a
#'   `tidyselect` expression like [any_of()] or [starts_with()]
#'   from `tidyselect` itself, or [tar_described_as()] to select target names
#'   based on their descriptions.
#' @param shortcut Logical of length 1, how to interpret the `names` argument.
#'   If `shortcut` is `FALSE` (default) then the function checks
#'   all targets upstream of `names` as far back as the dependency graph goes.
#'   `shortcut = TRUE` increases speed if there are a lot of
#'   up-to-date targets, but it assumes all the dependencies
#'   are up to date, so please use with caution.
#'   It relies on stored metadata for information about upstream dependencies.
#'   `shortcut = TRUE` only works if you set `names`.
#' @param reporter Character of length 1, name of the reporter to user.
#'   Controls how messages are printed as targets run in the pipeline.
#'
#'   The default value of `reporter` is the value
#'   returned by `tar_config_get("reporter_make")`.
#'   The default of `tar_config_get("reporter_make")` is `"terse"`
#'   if the calling R session is either:
#'
#'       1. Non-interactive (`interactive()` returns `FALSE`), or
#'       2. Inside a literate programming document
#'         (the `knitr.in.progress` global option is `TRUE`).
#'
#'   Otherwise, the default is `"balanced"`.
#'   You can always set the reporter manually.
#'   Choices:
#'
#'   * `"balanced"`: a reporter that balances efficiency
#'       with informative detail.
#'       Uses a `cli` progress bar instead of printing messages
#'       for individual dynamic branches.
#'       To the right of the progress bar is a text string like
#'       "22.6s, 4510+, 124-" (22.6 seconds elapsed, 4510 targets
#'       completed successfully so far, 124 targets skipped so far).
#'
#'       For best results with the balanced reporter, you may need to
#'       adjust your `cli` settings. See global options `cli.num_colors`
#'       and `cli.dynamic` at
#'       <https://cli.r-lib.org/reference/cli-config.html>.
#'       On that page is also the `CLI_TICK_TIME` environment variable
#'       which controls the time delay between progress bar updates.
#'       If the delay is too low, then overhead from printing to the console
#'       may slow down the pipeline.
#'   * `"terse"`: like the `"balanced"` reporter, but without a progress bar.
#'   * `"silent"`: print nothing.
#'   * `"timestamp"`: same as the `"verbose"` reporter except that each
#'      message begins with a time stamp.
#'   * `"verbose"`: print messages for individual targets
#'     as they dispatch or complete. Each individual
#'     target-specific time (e.g. "3.487 seconds") is strictly the
#'     elapsed runtime of the target and does not include
#'     steps like data retrieval and output storage.
#' @param seconds_interval Deprecated on 2023-08-24
#'   (targets version 1.2.2.9001).
#'   Use `seconds_meta_append` and `seconds_meta_upload` instead.
#' @param seconds_meta_append Positive numeric of length 1 with the minimum
#'   number of seconds between saves to the local metadata and progress files
#'   in the data store.
#'   his is an aggressive optimization setting not recommended
#'   for most users:
#'   higher values generally make the pipeline run faster, but unsaved
#'   work (in the event of a crash) is not up to date.
#'   When the pipeline ends,
#'   all the metadata and progress data is saved immediately,
#'   regardless of `seconds_meta_append`.
#'
#'   When the pipeline is just skipping targets, the actual interval
#'   between saves is `max(1, seconds_meta_append)` to reduce
#'   overhead.
#' @param seconds_meta_upload Positive numeric of length 1 with the minimum
#'   number of seconds between uploads of the metadata and progress data
#'   to the cloud
#'   (see <https://books.ropensci.org/targets/cloud-storage.html>).
#'   Higher values generally make the pipeline run faster, but unsaved
#'   work (in the event of a crash) may not be backed up to the cloud.
#'   When the pipeline ends,
#'   all the metadata and progress data is uploaded immediately,
#'   regardless of `seconds_meta_upload`.
#' @param seconds_reporter Deprecated on 2025-03-31
#'   (`targets` version 1.10.1.9010).
#' @param garbage_collection Deprecated. Use the `garbage_collection`
#'   argument of [tar_option_set()] instead to run garbage collection
#'   at regular intervals in a pipeline, or use the argument of the same
#'   name in [tar_target()] to activate garbage collection for
#'   a specific target.
#' @param use_crew Logical of length 1, whether to use `crew` if the
#'   `controller` option is set in `tar_option_set()` in the target script
#'   (`_targets.R`). See <https://books.ropensci.org/targets/crew.html>
#'   for details.
#' @param terminate_controller Logical of length 1. For a `crew`-integrated
#'   pipeline, whether to terminate the controller after stopping
#'   or finishing the pipeline. This should almost always be set to `TRUE`,
#'   but `FALSE` combined with `callr_function = NULL`
#'   will allow you to get the running controller using
#'   `tar_option_get("controller")` for debugging purposes.
#'   For example, `tar_option_get("controller")$summary()` produces a
#'   worker-by-worker summary of the work assigned and completed,
#'   `tar_option_get("controller")$queue` is the list of unresolved tasks,
#'   and `tar_option_get("controller")$results` is the list of
#'   tasks that completed but were not collected with `pop()`.
#'   You can manually terminate the controller with
#'   `tar_option_get("controller")$summary()` to close down the dispatcher
#'   and worker processes.
#' @param as_job `TRUE` to run as an RStudio IDE / Posit Workbench job,
#'   if running on RStudio IDE / Posit Workbench.
#'   `FALSE` to run as a `callr` process in the main R session
#'   (depending on the `callr_function` argument).
#'   If `as_job` is `TRUE`, then the `rstudioapi` package must be installed.
#' @examples
#' if (identical(Sys.getenv("TAR_EXAMPLES"), "true")) { # for CRAN
#' tar_dir({ # tar_dir() runs code from a temp dir for CRAN.
#' tar_script({
#'   library(targets)
#'   library(tarchetypes)
#'   list(
#'     tar_target(y1, 1 + 1),
#'     tar_target(y2, 1 + 1),
#'     tar_target(z, y1 + y2)
#'   )
#' }, ask = FALSE)
#' tar_make(starts_with("y")) # Only processes y1 and y2.
#' # Distributed computing with crew:
#' if (requireNamespace("crew", quietly = TRUE)) {
#' tar_script({
#'   library(targets)
#'   library(tarchetypes)
#'   tar_option_set(controller = crew::controller_local())
#'   list(
#'     tar_target(y1, 1 + 1),
#'     tar_target(y2, 1 + 1),
#'     tar_target(z, y1 + y2)
#'   )
#' }, ask = FALSE)
#' tar_make()
#' }
#' })
#' }
tar_make <- function(
  names = NULL,
  shortcut = targets::tar_config_get("shortcut"),
  reporter = targets::tar_config_get("reporter_make"),
  seconds_meta_append = targets::tar_config_get("seconds_meta_append"),
  seconds_meta_upload = targets::tar_config_get("seconds_meta_upload"),
  seconds_reporter = targets::tar_config_get("seconds_reporter"),
  seconds_interval = targets::tar_config_get("seconds_interval"),
  callr_function = callr::r,
  callr_arguments = targets::tar_callr_args_default(callr_function, reporter),
  envir = parent.frame(),
  script = targets::tar_config_get("script"),
  store = targets::tar_config_get("store"),
  garbage_collection = NULL,
  use_crew = targets::tar_config_get("use_crew"),
  terminate_controller = TRUE,
  as_job = targets::tar_config_get("as_job")
) {
  tar_assert_allow_meta("tar_make", store)
  force(envir)
  tar_assert_scalar(shortcut)
  tar_assert_lgl(shortcut)
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
  tar_assert_lgl(terminate_controller)
  tar_assert_scalar(terminate_controller)
  tar_assert_none_na(terminate_controller)
  tar_assert_lgl(as_job)
  tar_assert_scalar(as_job)
  tar_assert_none_na(as_job)
  reporter <- tar_make_reporter(reporter)
  if_any(
    is.null(garbage_collection),
    NULL,
    tar_warn_deprecate(
      "The garbage_collection argument of tar_make() was deprecated ",
      "in targets version 1.8.0.9004 (2024-10-22). The garbage_collection ",
      "argument of tar_option_set() is more unified and featureful now. ",
      "Please have a look at its documentation."
    )
  )
  if_any(
    is.null(seconds_reporter),
    NULL,
    tar_warn_deprecate(
      "The seconds_reporter argument of tar_make() etc. was deprecated ",
      "in targets version 1.10.1.9010 (2025-03-31)."
    )
  )
  # Tested in tests/interactive/test-job.R.
  # nocov start
  if (as_job && !rstudio_available(verbose = reporter != "silent")) {
    as_job <- FALSE
  }
  if (as_job) {
    call <- match.call()
    tar_make_as_job(call = call, reporter = reporter)
    return(invisible())
  }
  # nocov end
  targets_arguments <- list(
    path_store = store,
    names_quosure = rlang::enquo(names),
    shortcut = shortcut,
    reporter = reporter,
    seconds_meta_append = seconds_meta_append,
    seconds_meta_upload = seconds_meta_upload,
    use_crew = use_crew,
    terminate_controller = terminate_controller
  )
  out <- callr_outer(
    targets_function = tar_make_inner,
    targets_arguments = targets_arguments,
    callr_function = callr_function,
    callr_arguments = callr_arguments,
    envir = envir,
    script = script,
    store = store,
    fun = "tar_make"
  )
  invisible(out)
}

tar_make_inner <- function(
  pipeline,
  path_store,
  names_quosure,
  shortcut,
  reporter,
  seconds_meta_append,
  seconds_meta_upload,
  use_crew,
  terminate_controller
) {
  names <- tar_tidyselect_eval(names_quosure, pipeline_get_names(pipeline))
  controller <- tar_option_get("controller")
  if (is.null(controller) || (!use_crew)) {
    pipeline_reset_deployments(pipeline)
    queue <- if_any(
      pipeline_uses_priorities(pipeline),
      "parallel",
      "sequential"
    )
    local_init(
      pipeline = pipeline,
      meta = meta_init(path_store = path_store),
      names = names,
      shortcut = shortcut,
      queue = queue,
      reporter = reporter,
      seconds_meta_append = seconds_meta_append,
      seconds_meta_upload = seconds_meta_upload,
      envir = tar_option_get("envir")
    )$run()
  } else {
    tar_assert_package("crew (>= 0.9.0)")
    crew_init(
      pipeline = pipeline,
      meta = meta_init(path_store = path_store),
      names = names,
      shortcut = shortcut,
      queue = "parallel",
      reporter = reporter,
      seconds_meta_append = seconds_meta_append,
      seconds_meta_upload = seconds_meta_upload,
      envir = tar_option_get("envir"),
      controller = controller,
      terminate_controller = terminate_controller
    )$run()
  }
  invisible()
}

tar_make_reporter <- function(reporter) {
  reporter <- as.character(reporter)
  template <- paste0(
    "The \"%s\" reporter ",
    "in tar_make() etc. was deprecated ",
    "in targets version 1.10.1.9010 (2025-03-31). ",
    "Use the \"balanced\", \"verbose\", or \"timestamp\" ",
    "reporter instead. Switching to \"%s\"."
  )
  if (identical(reporter, "summary")) {
    tar_warn_deprecate(sprintf(template, "summary", "balanced"))
    reporter <- "balanced"
  }
  if (identical(reporter, "timestamp_positives")) {
    tar_warn_deprecate(
      sprintf(template, "timestamp_positives", "timestamp")
    )
    reporter <- "timestamp"
  }
  if (identical(reporter, "verbose_positives")) {
    tar_warn_deprecate(
      sprintf(template, "verbose_positives", "verbose")
    )
    reporter <- "verbose"
  }
  tar_assert_flag(reporter, tar_reporters_make())
  reporter
}

# Tested in tests/interactive/test-job.R.
# nocov start
tar_make_as_job <- function(call, reporter) {
  args <- as.list(call)[-1L]
  args$as_job <- FALSE
  args$callr_function <- NULL
  args$reporter <- reporter
  args <- paste(names(args), "=", map_chr(args, tar_deparse_safe))
  args <- c(args, "callr_function = NULL")
  args <- paste0(args, collapse = ", ")
  text <- sprintf("targets::tar_make(%s)", args)
  script <- tempfile()
  writeLines(text, script)
  rstudioapi::jobRunScript(
    path = script,
    name = paste("targets", Sys.time()),
    workingDir = getwd(),
    importEnv = FALSE,
    exportEnv = ""
  )
}
# nocov end
