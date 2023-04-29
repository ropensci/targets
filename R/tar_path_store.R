#' @title Current data store path
#' @export
#' @family utilities
#' @description Identify the file path to the data store
#'  of the pipeline currently running.
#' @return Character, file path to the data store
#'   of the pipeline currently running.
#'   If called outside of the pipeline currently running,
#'   `tar_path_store()` returns `tar_config_get("store")`.
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
tar_path_store <- function() {
  if_any(
    !is.null(tar_runtime$store),
    tar_runtime$store,
    tar_config_get("store")
  )
}
