#' @title Check which targets are outdated.
#' @export
#' @family inspect
#' @description Checks for outdated targets in the pipeline,
#'   targets that will be rerun automatically if you call
#'   [tar_make()] or similar. See [tar_cue()] for the rules
#'   that decide whether a target needs to rerun.
#' @details Requires that you define a pipeline
#'   with a target script file (default: `_targets.R`).
#'   (See [tar_script()] for details.)
#' @inheritSection tar_meta Storage access
#' @return Names of the outdated targets.
#' @inheritParams tar_make
#' @param names Names of the targets. `tar_outdated()` will check
#'   these targets and all upstream ancestors in the dependency graph.
#'   Set `names` to `NULL` to check/build all the targets (default).
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
#'   Also, `shortcut = TRUE` only works if you set `names`.
#' @param branches Logical of length 1, whether to include branch names.
#'   Including branches could get cumbersome for large pipelines.
#'   Individual branch names are still omitted when branch-specific information
#'   is not reliable: for example, when a pattern branches over
#'   an outdated target.
#' @param targets_only Logical of length 1, whether to just restrict to targets
#'   or to include functions and other global objects from the environment
#'   created by running the target script file (default: `_targets.R`).
#' @param reporter Character of length 1, name of the reporter to user.
#'   Controls how messages are printed as targets are checked.
#'
#'   The default of `tar_config_get("reporter_make")` is `"terse"`
#'   if running inside a literate programming document
#'   (i.e. the `knitr.in.progress` global option is `TRUE`).
#'   Otherwise, the default is `"balanced"`. Choices:
#'
#'     * `"balanced"`: a reporter that balances efficiency
#'       with informative detail.
#'       Uses a `cli` progress bar instead of printing messages
#'       for individual dynamic branches.
#'       To the right of the progress bar is a text string like
#'       "22.6s, 4510+, 124-" (22.6 seconds elapsed, 4510 targets
#'       detected as outdated so far,
#'       124 targets detected as up to date so far).
#'
#'       For best results with the balanced reporter, you may need to
#'       adjust your `cli` settings. See global options `cli.num_colors`
#'       and `cli.dynamic` at
#'       <https://cli.r-lib.org/reference/cli-config.html>.
#'       On that page is also the `CLI_TICK_TIME` environment variable
#'       which controls the time delay between progress bar updates.
#'       If the delay is too low, then overhead from printing to the console
#'       may slow down the pipeline.
#'     * `"terse"`: like `"balanced"`, except without a progress bar.
#'     * `"silent"`: print nothing.
#' @inheritParams tar_validate
#' @examples
#' if (identical(Sys.getenv("TAR_EXAMPLES"), "true")) { # for CRAN
#' tar_dir({ # tar_dir() runs code from a temp dir for CRAN.
#' tar_script(list(tar_target(x, 1 + 1)))
#' tar_outdated()
#' tar_script({
#'   library(targets)
#'   library(tarchetypes)
#'   list(
#'     tar_target(y1, 1 + 1),
#'     tar_target(y2, 1 + 1),
#'     tar_target(z, y1 + y2)
#'   )
#' }, ask = FALSE)
#' tar_outdated()
#' })
#' }
tar_outdated <- function(
  names = NULL,
  shortcut = targets::tar_config_get("shortcut"),
  branches = FALSE,
  targets_only = TRUE,
  reporter = targets::tar_config_get("reporter_outdated"),
  seconds_reporter = targets::tar_config_get("seconds_reporter_outdated"),
  seconds_interval = targets::tar_config_get("seconds_interval"),
  callr_function = callr::r,
  callr_arguments = targets::tar_callr_args_default(callr_function, reporter),
  envir = parent.frame(),
  script = targets::tar_config_get("script"),
  store = targets::tar_config_get("store")
) {
  if_any(
    is.null(seconds_reporter),
    NULL,
    tar_warn_deprecate(
      "The seconds_reporter argument of tar_outdated() etc. was deprecated ",
      "in targets version 1.10.1.9010 (2025-03-31)."
    )
  )
  tar_assert_allow_meta("tar_outdated", store)
  force(envir)
  tar_assert_scalar(shortcut)
  tar_assert_lgl(shortcut)
  tar_assert_lgl(branches)
  reporter <- tar_outdated_reporter(reporter)
  tar_deprecate_seconds_interval(seconds_interval)
  tar_assert_callr_function(callr_function)
  tar_assert_list(callr_arguments)
  targets_arguments <- list(
    path_store = store,
    names_quosure = rlang::enquo(names),
    shortcut = shortcut,
    branches = branches,
    targets_only = targets_only,
    reporter = reporter
  )
  callr_outer(
    targets_function = tar_outdated_inner,
    targets_arguments = targets_arguments,
    callr_function = callr_function,
    callr_arguments = callr_arguments,
    envir = envir,
    script = script,
    store = store,
    fun = "tar_outdated"
  )
}

tar_outdated_inner <- function(
  pipeline,
  path_store,
  names_quosure,
  shortcut,
  branches,
  targets_only,
  reporter
) {
  names_all <- pipeline_get_names(pipeline)
  names <- tar_tidyselect_eval(names_quosure, names_all)
  meta <- meta_init(path_store = path_store)
  outdated_globals <- if_any(
    targets_only,
    character(0),
    tar_outdated_globals(pipeline, meta)
  )
  outdated <- outdated_init(
    pipeline = pipeline,
    meta = meta_init(path_store = path_store),
    names = names,
    shortcut = shortcut,
    queue = "sequential",
    reporter = reporter
  )
  outdated$run()
  outdated_targets <- counter_get_names(outdated$outdated)
  if (!branches) {
    outdated_targets <- intersect(outdated_targets, names_all)
  }
  c(outdated_globals, outdated_targets)
}

tar_outdated_globals <- function(pipeline, meta) {
  meta$ensure_preprocessed(write = FALSE)
  new <- hash_imports(pipeline$imports)
  new$new <- new$data
  recorded <- fltr(new$name, ~meta$exists_record(.x))
  if (!length(recorded)) {
    return(new$name)
  }
  data <- map_chr(recorded, ~meta$get_record(.x)$data)
  old <- utils::stack(data)
  old$name <- as.character(old$ind)
  old$old <- old$values
  comparison <- merge(new, old, all.x = TRUE)
  comparison$values[is.na(comparison$values)] <- ""
  different <- comparison$new != comparison$old
  different[is.na(different)] <- TRUE
  comparison$name[different]
}

tar_outdated_reporter <- function(reporter) {
  tar_config_assert_reporter_outdated(reporter)
  if_any(
    reporter %in% c("forecast", "forecast_interactive"),
    "balanced",
    reporter
  )
}
