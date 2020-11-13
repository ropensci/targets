#' @title List saved targets
#' @export
#' @description List targets currently saved to `_targets/objects/`.
#'   Does not include dynamic files or cloud storage.
#' @examples
#' if (FALSE) {
#' tar_dir({
#' tar_script({
#'   tar_option_set(workspace = "x")
#'   tar_pipeline(tar_target(x, "value"))
#' })
#' tar_make()
#' tar_workspaces()
#' })
#' }
tar_workspaces <- function() {
  trn(
    dir.exists(path_default_dir()),
    sort(list.files(path_default_dir(), all.files = TRUE, no.. = TRUE)),
    character(0)
  )
}
