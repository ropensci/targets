#' @title Check if process metadata exists.
#' @export
#' @family existence
#' @description Check if the process metadata file `_targets/meta/process`
#'   exists for the current project.
#' @details To learn more about data storage in `targets`, visit
#'   <https://books.ropensci.org/targets/data.html>.
#' @return Logical of length 1, whether the current project's metadata exists.
#' @inheritParams tar_validate
#' @examples
#' tar_exist_process()
tar_exist_process <- function(store = targets::tar_config_get("store")) {
  tar_assert_allow_meta("tar_exist_process", store)
  file.exists(path_process(path_store = store))
}
