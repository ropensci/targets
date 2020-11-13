#' @title List saved target workspaces.
#' @export
#' @description List target workspaces currently saved to
#'   `_targets/workspaces/`. See [tar_workspace()] for more information.
#' @examples
#' if (identical(Sys.getenv("TARGETS_LONG_EXAMPLES"), "true")) {
#' tar_dir({
#' tar_script({
#'   tar_option_set(error = "save") # Required for saving workspaces.
#'   tar_pipeline(
#'     tar_target(x, "value"),
#'     tar_target(y, x)
#'   )
#' })
#' tar_make()
#' tar_workspaces()
#' })
#' }
tar_workspaces <- function() {
  trn(
    dir.exists(path_workspaces_dir()),
    sort(list.files(path_workspaces_dir(), all.files = TRUE, no.. = TRUE)),
    character(0)
  )
}
