#' @title List errored targets.
#' @export
#' @family progress
#' @description List targets whose progress is `"errored"`.
#' @return A character vector of errored targets.
#' @inheritSection tar_meta Storage access
#' @inheritParams tar_progress
#' @param names Optional, names of the targets. If supplied, the
#'   function restricts its output to these targets.
#'   You can supply symbols
#'   or `tidyselect` helpers like [any_of()] and [starts_with()].
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
#' tar_errored()
#' tar_errored(starts_with("y_")) # see also any_of()
#' })
#' }
tar_errored <- function(
  names = NULL,
  store = targets::tar_config_get("store")
) {
  tar_assert_allow_meta("tar_errored")
  progress <- progress_init(path_store = store)
  progress <- tibble::as_tibble(progress$database$read_condensed_data())
  names_quosure <- rlang::enquo(names)
  names <- tar_tidyselect_eval(names_quosure, progress$name)
  if (!is.null(names)) {
    progress <- progress[match(names, progress$name), , drop = FALSE] # nolint
  }
  progress$name[progress$progress == "errored"]
}
