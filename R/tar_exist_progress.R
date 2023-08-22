#' @title Check if progress metadata exists.
#' @export
#' @family existence
#' @description Check if the progress metadata file `_targets/meta/progress`
#'   exists for the current project.
#' @details To learn more about data storage in `targets`, visit
#'   <https://books.ropensci.org/targets/data.html>.
#' @return Logical of length 1, whether the current project's metadata exists.
#' @inheritParams tar_validate
#' @examples
#' tar_exist_progress()
tar_exist_progress <- function(store = targets::tar_config_get("store")) {
  tar_assert_allow_meta("tar_exist_progress")
  file.exists(path_progress(path_store = store))
}
