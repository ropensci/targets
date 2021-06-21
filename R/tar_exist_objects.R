#' @title Check if local output data exists for one or more targets.
#' @export
#' @family existence
#' @description Check if the local data files exist in
#'   `_targets/objects/` for one or more targets.
#' @details To learn more about local storage in `targets`, visit
#'   <https://books.ropensci.org/targets/files.html#internal-files>.
#' @return Logical of length `length(names)`, whether
#'   each given target has an existing file in `_targets/objects/`
#'   for the current project.
#' @inheritParams tar_validate
#' @param names Character vector of target names.
#' @examples
#' tar_exist_objects(c("target1", "target2"))
tar_exist_objects <- function(
  names,
  store = targets::tar_config_get("store")
) {
  tar_assert_chr(names, "names must be a character vector.")
  file.exists(path_objects(path_store = store, name = names))
}
