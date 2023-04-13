#' @title Deprecated: current data store path
#' @export
#' @keywords internal
#' @family utilities
#' @description Deprecated: identify the file path to the data store
#'   of the pipeline currently running.
#' @details `tar_store()` was deprecated on 2022-10-11 (version 0.13.5.9000).
#'   Use [tar_path_store()] instead.
#' @return Character, file path to the data store
#'   of the pipeline currently running.
#'   If called outside of the pipeline currently running,
#'   `tar_store()` returns `tar_config_get("store")`.
#' @examples
#' tar_path_store()
#' if (identical(Sys.getenv("TAR_EXAMPLES"), "true")) { # for CRAN
#' tar_dir({ # tar_dir() runs code from a temp dir for CRAN.
#' tar_script(tar_target(x, tar_path_store()), ask = FALSE)
#' store <- tempfile()
#' tar_make(store = store)
#' tar_read(x, store = store)
#' })
#' }
tar_store <- function() {
  tar_warn_deprecate(
    "tar_store() is deprecated. Use tar_path_store() instead."
  )
  tar_path_store()
}
