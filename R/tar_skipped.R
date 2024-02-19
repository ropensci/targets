#' @title List skipped targets.
#' @export
#' @family progress
#' @description List targets whose progress is `"skipped"`.
#' @return A character vector of skipped targets.
#' @inheritParams tar_progress
#' @examples
#' if (identical(Sys.getenv("TAR_EXAMPLES"), "true")) { # for CRAN
#' tar_dir({ # tar_dir() runs code from a temp dir for CRAN.
#' tar_script({
#'   list(
#'     tar_target(x, seq_len(2)),
#'     tar_target(y, 2 * x, pattern = map(x))
#'   )
#' }, ask = FALSE)
#' tar_make()
#' tar_skipped()
#' tar_skipped(starts_with("y_")) # see also any_of()
#' })
#' }
tar_skipped <- function(
  names = NULL,
  store = targets::tar_config_get("store")
) {
  tar_assert_allow_meta("tar_skipped", store)
  progress <- progress_init(path_store = store)
  progress <- tibble::as_tibble(progress$database$read_condensed_data())
  names_quosure <- rlang::enquo(names)
  names <- tar_tidyselect_eval(names_quosure, progress$name)
  if (!is.null(names)) {
    progress <- progress[match(names, progress$name), , drop = FALSE] # nolint
  }
  progress$name[progress$progress == "skipped"]
}
