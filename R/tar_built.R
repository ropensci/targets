#' @title Deprecated: list built targets.
#' @export
#' @keywords internal
#' @description Deprecated in favor of [tar_completed()] on 2023-12-04
#'   (version 1.3.2.9004).
#' @return A character vector of completed targets.
#' @inheritParams tar_progress
#' @examples
#' if (identical(Sys.getenv("TAR_EXAMPLES"), "true")) { # for CRAN
#' tar_dir({ # tar_dir() runs code from a temp dir for CRAN.
#' tar_script({
#'   library(targets)
#'   library(tarchetypes)
#'   list(
#'     tar_target(x, seq_len(2)),
#'     tar_target(y, 2 * x, pattern = map(x))
#'   )
#' }, ask = FALSE)
#' tar_make()
#' tar_completed()
#' tar_completed(starts_with("y_")) # see also any_of()
#' })
#' }
tar_built <- function(
  names = NULL,
  store = targets::tar_config_get("store")
) {
  tar_warn_deprecate(
    "tar_built() is deprecated in targets version >= 1.3.2.9004 ",
    "(2021-12-04). Use tar_completed() instead."
  )
  tar_assert_allow_meta("tar_built", store)
  progress <- progress_init(path_store = store)
  progress <- tibble::as_tibble(progress$database$read_condensed_data())
  names_quosure <- rlang::enquo(names)
  names <- tar_tidyselect_eval(names_quosure, progress$name)
  if (!is.null(names)) {
    progress <- progress[match(names, progress$name), , drop = FALSE] # nolint
  }
  progress$name[progress$progress == "completed"]
}
