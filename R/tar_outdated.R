#' @title Check which targets are outdated.
#' @export
#' @description Checks for outdated targets in the pipeline,
#'   targets that will be rerun automatically if you call
#'   [tar_make()] or similar. See [tar_cue()] for the rules
#'   that decide whether a target needs to rerun.
#' @details Requires that you define a pipeline
#'   with a `_targets.R` script in your working directory.
#'   (See [tar_script()] for details.)
#' @return Names of the outdated targets.
#' @param names Names of the targets. `tar_outdated()` will check
#'   these targets and all upstream ancestors in the dependency graph.
#'   Set `names` to `NULL` to check/build all the targets (default).
#'   Otherwise, you can supply symbols, a character vector,
#'   or `tidyselect` helpers like [starts_with()].
#' @param branches Logical, whether to include branch names.
#'   Including branches could get cumbersome for large pipelines.
#'   Individual branch names are still omitted when branch-specific information
#'   is not reliable: for example, when a pattern branches over
#'   an outdated target.
#' @param targets_only Logical, whether to just restrict to targets
#'   or to include functions and other global objects from the environment
#'   created by running `_targets.R`.
#' @param reporter Character of length 1, name of the reporter to user.
#'   Controls how messages are printed as targets are checked. Choices:
#'   * `"silent"`: print nothing.
#'   * `"forecast"`: print running totals of the checked and outdated
#'     targets found so far.
#' @inheritParams tar_validate
#' @examples
#' if (identical(Sys.getenv("TARGETS_LONG_EXAMPLES"), "true")) {
#' tar_dir({ # Write all files to a temporary directory.
#' tar_script(list(tar_target(x, 1 + 1)))
#' tar_outdated()
#' tar_script(
#'   list(
#'     tar_target(y1, 1 + 1),
#'     tar_target(y2, 1 + 1),
#'     tar_target(z, y1 + y2)
#'   )
#' )
#' tar_outdated()
#' })
#' }
tar_outdated <- function(
  names = NULL,
  branches = FALSE,
  targets_only = TRUE,
  reporter = "silent",
  callr_function = callr::r,
  callr_arguments = list(spinner = identical(reporter, "silent"))
) {
  assert_target_script()
  assert_lgl(branches, "branches arg of tar_outdated() must be logical.")
  assert_scalar(reporter, "reporter arg of tar_outdated() must have length 1.")
  assert_in(
    reporter,
    c("forecast", "silent"),
    "reporter arg of tar_outdated() must either be \"silent\" or \"forecast\""
  )
  assert_callr_function(callr_function)
  assert_list(callr_arguments, "callr_arguments mut be a list.")
  targets_arguments <- list(
    names_quosure = rlang::enquo(names),
    branches = branches,
    targets_only = targets_only,
    reporter = reporter
  )
  callr_outer(
    targets_function = tar_outdated_inner,
    targets_arguments = targets_arguments,
    callr_function = callr_function,
    callr_arguments = callr_arguments
  )
}

tar_outdated_inner <- function(
  pipeline,
  names_quosure,
  branches,
  targets_only,
  reporter
) {
  names_all <- pipeline_get_names(pipeline)
  names <- eval_tidyselect(names_quosure, names_all)
  meta <- meta_init()
  outdated_globals <- trn(
    targets_only,
    character(0),
    tar_outdated_globals(pipeline, meta)
  )
  outdated <- outdated_init(
    pipeline = pipeline,
    names = names,
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
  meta$database$ensure_preprocessed(write = FALSE)
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
