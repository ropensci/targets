#' @title Remove debugging files.
#' @export
#' @description Destroy `_targets/workspaces/` and `_targets/scratch/`,
#'   which are directories with files for debugging.
#' @return Nothing.
#' @examples
#' \dontrun{
#' tar_script({
#'   tar_option_set(error = "workspace") # Required for saving workspaces.
#'   tar_pipeline(
#'     tar_target(x, "loaded"),
#'     tar_target(y, stop(x))
#'   )
#' })
#' try(tar_make())
#' file.exists("_targets/workspaces/y") # TRUE
#' tar_workspaces_destroy()
#' file.exists("_targets/workspaces/y") # FALSE
#' }
tar_workspaces_destroy <- function() {
  unlink(path_scratch_dir(), recursive = TRUE)
  unlink(path_workspaces_dir(), recursive = TRUE)
  invisible()
}
