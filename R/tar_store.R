#' @title Current data store path
#' @export
#' @family utilities
#' @description Identify the file path to the data store
#'  of the pipeline currently running.
#' @return Character, file path to the data store
#'   of the pipeline currently running.
#'   If called outside of the pipeline currently running,
#'   `tar_store()` returns `tar_config_get("store")`.
#' @examples
#' tar_store()
#' if (identical(Sys.getenv("TAR_EXAMPLES"), "true")) {
#' tar_dir({ # tar_dir() runs code from a temporary directory.
#' tar_script(tar_target(x, tar_store()), ask = FALSE)
#' store <- tempfile()
#' tar_make(store = store)
#' tar_read(x, store = store)
#' })
#' }
tar_store <- function() {
  if_any(
    tar_runtime$exists_store(),
    tar_runtime$get_store(),
    tar_config_get("store")
  )
}
