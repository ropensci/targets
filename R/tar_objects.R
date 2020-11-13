#' @title List saved targets
#' @export
#' @description List targets currently saved to `_targets/objects/`.
#'   Does not include dynamic files or cloud storage.
#' @examples
#' if (FALSE) {
#' tar_dir({
#' tar_script({
#'   tar_option_set(error = "save") # Required for saving workspaces.
#'   tar_pipeline(
#'     tar_target(x, "value"),
#'     tar_target(y, x)
#'   )
#' })
#' tar_make()
#' tar_objects()
#' })
#' }
tar_objects <- function() {
  trn(
    dir.exists(path_default_dir()),
    sort(list.files(path_default_dir(), all.files = TRUE, no.. = TRUE)),
    character(0)
  )
}
