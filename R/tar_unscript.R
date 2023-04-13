#' @title Remove target script helper files.
#' @export
#' @description Remove target script helper files (default: `_targets_r/`)
#'   that were created by Target Markdown.
#' @details Target Markdown code chunks create R scripts in a folder
#'   called `_targets_r/` in order to aid the automatically supplied
#'   `_targets.R` file. Over time, the number of script files
#'   starts to build up, and `targets` has no way of automatically
#'   removing helper script files that are no longer necessary.
#'   To keep your pipeline up to date
#'   with the code chunks in the Target Markdown document(s),
#'   it is good practice to call `tar_unscript()` at the beginning
#'   of your first Target Markdown document. That way,
#'   extraneous/discarded targets are automatically
#'   removed from the pipeline when the document starts render.
#'
#'   If the target script is at some alternative path,
#'   e.g. `custom/script.R`, the helper scripts are in `custom/script_r/`.
#'   [tar_unscript()] works on the helper scripts as long as your
#'   project configuration settings correctly identify the correct
#'   target script.
#' @return `NULL` (invisibly).
#' @inheritParams tar_validate
#' @examples
#' tar_dir({ # tar_dir() runs code from a temp dir for CRAN.
#' tar_unscript()
#' })
tar_unscript <- function(script = targets::tar_config_get("script")) {
  if (!tar_exist_script(script = script)) {
    return(invisible())
  }
  unlink(path_script_r(script), recursive = TRUE)
  invisible()
}
