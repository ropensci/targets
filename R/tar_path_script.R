#' @title Current target script path
#' @export
#' @family utilities
#' @description Identify the file path to the target script
#'  of the pipeline currently running.
#' @return Character, file path to the target script
#'   of the pipeline currently running.
#'   If called outside of the pipeline currently running,
#'   `tar_path_script()` returns `tar_config_get("script")`.
#' @examples
#' tar_path_script()
#' if (identical(Sys.getenv("TAR_EXAMPLES"), "true")) { # for CRAN
#' tar_dir({ # tar_dir() runs code from a temp dir for CRAN.
#' script <- tempfile()
#' tar_script(tar_target(x, tar_path_script()), script = script, ask = FALSE)
#' tar_make(script = script)
#' tar_read(x)
#' })
#' }
tar_path_script <- function() {
  if_any(
    tar_runtime$exists_script(),
    tar_runtime$get_script(),
    tar_config_get("script")
  )
}
