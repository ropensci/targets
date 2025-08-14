#' @title Select targets using their descriptions.
#' @export
#' @family utilities
#' @description Select a subset of targets in the `_targets.R` file
#'   based on their custom descriptions.
#' @details Targets with empty descriptions are ignored.
#' @return If `tidyselect` is `TRUE`, then [tar_described_as()] returns
#'   a call to `tidyselect::all_of()` which can be supplied to the `names`
#'   argument of functions like [tar_manifest()] and [tar_make()].
#'   This allows functions like [tar_manifest()] and [tar_make()]
#'   to focus on only the targets with the matching descriptions.
#'   If `tidyselect` is `FALSE`, then [tar_described_as()] returns
#'   a simple character vector of the names of all the targets in the
#'   pipeline with matching descriptions.
#' @inheritParams tar_validate
#' @param described_as A `tidyselect` expression to select targets
#'   based on their descriptions. For example,
#'   `described_as = starts_with("survival model")` matches all
#'   targets in the pipeline whose `description` arguments
#'   of [tar_target()] start with the text string `"survival model"`.
#' @param tidyselect If `TRUE`, return a call to `tidyselect::all_of()`
#'   identifying the selected targets, which can then be supplied to any
#'   `tidyselect`-compatible names` argument of downstream functions
#'   like [tar_make()] and [tar_manifest()].
#'   If `FALSE`, return a simple character vector of target names.
#' @examples
#' if (identical(Sys.getenv("TAR_EXAMPLES"), "true")) { # for CRAN
#' tar_dir({ # tar_dir() runs code from a temp dir for CRAN.
#' tar_script({
#'   library(targets)
#'   library(tarchetypes)
#'   list(
#'     tar_target(b2, TRUE, description = "blue two"),
#'     tar_target(b3, TRUE, description = "blue three"),
#'     tar_target(g2, TRUE, description = "green two"),
#'     tar_target(g3, TRUE, description = "green three"),
#'     tar_target(g4, TRUE, description = "green three")
#'   )
#' }, ask = FALSE)
#' tar_described_as(starts_with("green"), tidyselect = FALSE)
#' tar_make(names = tar_described_as(starts_with("green")))
#' tar_progress() # Only `g2`, `g3`, and `g4` ran.
#' })
#' }
tar_described_as <- function(
  described_as = NULL,
  tidyselect = TRUE,
  callr_function = callr::r,
  callr_arguments = targets::tar_callr_args_default(callr_function),
  envir = parent.frame(),
  script = targets::tar_config_get("script")
) {
  force(envir)
  tar_assert_envir(envir)
  tar_assert_lgl(tidyselect)
  tar_assert_scalar(tidyselect)
  tar_assert_none_na(tidyselect)
  tar_assert_callr_function(callr_function)
  tar_assert_list(callr_arguments)
  tar_assert_script(script)
  out <- callr_outer(
    targets_function = tar_described_as_inner,
    targets_arguments = list(
      described_as_quosure = rlang::enquo(described_as),
      tidyselect = tidyselect
    ),
    callr_function = callr_function,
    callr_arguments = callr_arguments,
    envir = envir,
    script = script,
    store = tempfile(),
    fun = "tar_described_as"
  )
  if_any(tidyselect, tidyselect::any_of(out), out)
}

tar_described_as_inner <- function(
  pipeline,
  described_as_quosure,
  tidyselect
) {
  names <- pipeline_get_names(pipeline)
  descriptions <- map(
    names,
    ~ pipeline_get_target(pipeline, .x)$settings$description
  )
  names(descriptions) <- names
  descriptions <- unlist(descriptions, use.names = TRUE)
  chosen <- tar_tidyselect_eval(described_as_quosure, unique(descriptions))
  sort(unique(names(descriptions[descriptions %in% chosen])))
}
