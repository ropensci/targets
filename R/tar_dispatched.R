#' @title List dispatched targets.
#' @export
#' @family progress
#' @description List the targets with progress status `"dispatched"`.
#' @details A target is `"dispatched"` if it is sent off to be run. Depending
#'   on your high-performance computing configuration via the `crew` package,
#'   the may not actually start right away. This may happen if the target
#'   is ready to start but all available parallel workers are busy.
#' @return A character vector of dispatched targets.
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
#' tar_dispatched()
#' tar_dispatched(starts_with("y_")) # see also any_of()
#' })
#' }
tar_dispatched <- function(
  names = NULL,
  store = targets::tar_config_get("store")
) {
  tar_assert_allow_meta("tar_dispatched", store)
  progress <- progress_init(path_store = store)
  progress <- tibble::as_tibble(progress$database$read_condensed_data())
  names_quosure <- rlang::enquo(names)
  names <- tar_tidyselect_eval(names_quosure, progress$name)
  if (!is.null(names)) {
    progress <- progress[match(names, progress$name), , drop = FALSE] # nolint
  }
  progress$name[progress$progress == "dispatched"]
}
