#' @title Remove target script helper files.
#' @export
#' @description Target Markdown writes helper scripts in a folder accompanying
#'   the target script file. If the target script is the default `_targets.R`,
#'   the helper scripts are in `_targets_r/`. If the some other location
#'   like `custom/script.R`, the helper scripts are in `custom/script_r/`.
#'   Use [tar_unscript()] to remove all these helper scripts in the `*_r/`
#'   directory. The actual target script is not removed.
#' @return `NULL` (invisibly).
#' @inheritParams tar_validate
#' @examples
#' tar_dir({ # tar_dir() runs code from a temporary directory.
#' tar_unscript()
#' })
tar_unscript <- function(script = targets::tar_config_get("script")) {
  if (!tar_exist_script(script = script)) {
    return(invisible())
  }
  unlink(path_script_r(script), recursive = TRUE)
  invisible()
}
