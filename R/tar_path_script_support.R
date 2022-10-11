#' @title Directory path to the support scripts
#'   of the current target script
#' @export
#' @family utilities
#' @description Identify the directory path to the
#'   support scripts
#'   of the current target script
#'   of the pipeline currently running.
#' @details A target script (default: `_targets.R`) comes with
#'   support scripts if it is written by Target Markdown.
#'   These support scripts usually live in a folder called `_targets_r/`,
#'   but the path may vary from case to case. The
#'   `tar_path_scipt_support()` returns the path to
#'   the folder with the support scripts.
#' @return Character, directory path to the target script
#'   of the pipeline currently running.
#'   If called outside of the pipeline currently running,
#'   `tar_path_script()` returns `tar_config_get("script")`.
#' @examples
#' tar_path_script_support()
#' if (identical(Sys.getenv("TAR_EXAMPLES"), "true")) {
#' tar_dir({ # tar_dir() runs code from a temporary directory.
#' script <- tempfile()
#' tar_script(
#'   tar_target(x, tar_path_script_support()),
#'   script = script,
#'   ask = FALSE
#' )
#' tar_make(script = script)
#' tar_read(x)
#' })
#' }
tar_path_script_support <- function() {
  paste0(tools::file_path_sans_ext(tar_path_script()), "_r")
}
