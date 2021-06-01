#' @title Check if progress metadata exists.
#' @export
#' @family existence
#' @description Check if the progress metadata file `_targets/meta/progress`
#'   exists for the current project.
#' @details To learn more about local storage in `targets`, visit
#'   <https://books.ropensci.org/targets/files.html#internal-files>.
#' @return Logical of length 1, whether the current project's metadata exists.
#' @inheritParams tar_validate
#' @examples
#' tar_exist_progress()
tar_exist_progress <- function(store = targets::tar_config_get("store")) {
  old_config <- switch_config(store = store, assert_store = FALSE)
  on.exit(restore_config(old_config), add = TRUE)
  file.exists(path_progress())
}
