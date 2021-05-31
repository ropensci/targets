#' @title Remove target script helper files.
#' @export
#' @description Target Markdown writes files to `_targets_r/`
#'   at the project root (next to `_targets.R`).
#'   Use [tar_unscript()] to remove all of `_targets_r/` and start with
#'   a fresh set of targets and globals to define the pipeline.
#' @return `NULL` (invisibly).
#' @examples
#' tar_dir({ # tar_dir() runs code from a temporary directory.
#' tar_unscript()
#' })
tar_unscript <- function() {
  unlink(path_script_r(), recursive = TRUE)
  invisible()
}
