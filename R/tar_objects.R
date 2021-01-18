#' @title List saved targets
#' @export
#' @description List targets currently saved to `_targets/objects/`.
#'   Does not include dynamic files or cloud storage.
#' @examples
#' if (identical(Sys.getenv("TARGETS_LONG_EXAMPLES"), "true")) {
#' tar_dir({ # Write all files to a temporary directory.
#' tar_script({
#'   tar_option_set(workspace = "x")
#'   list(tar_target(x, "value"))
#' })
#' tar_make()
#' tar_objects()
#' })
#' }
tar_objects <- function() {
  trn(
    dir.exists(path_objects_dir()),
    sort(list.files(path_objects_dir(), all.files = TRUE, no.. = TRUE)),
    character(0)
  )
}
