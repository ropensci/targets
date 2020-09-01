#' @title Remove debugging files.
#' @export
#' @description Destroy `_targets/workspaces/` and `_targets/scratch/`,
#'   which are directories with files for debugging. 
#' @return Nothing.
#' @examples
#' \dontrun{
#' tar_script({
#'   tar_option_set(error = "save") # Required for saving workspaces.
#'   tar_pipeline(
#'     tar_target(x, "loaded"),
#'     tar_target(y, stop(x))
#'   )
#' })
#' try(tar_make())
#' file.exists("_targets/workspaces/y") # TRUE
#' tar_undebug()
#' file.exists("_targets/workspaces/y") # FALSE
#' }
tar_undebug <- function() {
  unlink(store_dir_scratch(), recursive = TRUE)
  unlink(store_dir_workspaces(), recursive = TRUE)
  invisible()
}
