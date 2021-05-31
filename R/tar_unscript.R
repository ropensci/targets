#' @title Remove target script helper files.
#' @export
#' @description Target Markdown writes helper scripts in a folder accompanying
#'   the target script. If the target script is the default `_targets.R`,
#'   the helper scripts are in `_targets_r/`. If the some other location
#'   like `custom/script.R`, the helper scripts are in `custom/script_r/`.
#'   Use [tar_unscript()] to remove all these helper scripts.
#' @return `NULL` (invisibly).
#' @examples
#' tar_dir({ # tar_dir() runs code from a temporary directory.
#' tar_unscript()
#' })
tar_unscript <- function() {
  unlink(path_script_r(), recursive = TRUE)
  invisible()
}
