#' @title Check if target metadata exists.
#' @export
#' @family existence
#' @description Check if the target metadata file `_targets/meta/meta`
#'   exists for the current project.
#' @details To learn more about data storage in `targets`, visit
#'   <https://books.ropensci.org/targets/data.html>.
#' @return Logical of length 1, whether the current project's metadata exists.
#' @inheritParams tar_validate
#' @examples
#' tar_exist_meta()
tar_exist_meta <- function(store = targets::tar_config_get("store")) {
  file.exists(path_meta(path_store = store))
}
