#' @title Read optional settings in _targets.yaml
#' @export
#' @family configuration
#' @description Read the custom settings in the optional _targets.yaml
#'   configuration file of the current project.
#' @details `_targets.yaml` is an optional YAML configuration file
#'   with settings specific to a given project. In order to work properly,
#'   it must live at the root of the project (next to `_targets.R`).
#'   The settings currently supported are as follows. See the
#'   "YAML settings" section for descriptions of all supported settings.
#' @return A data frame of settings from `_targets.yaml`.
#'   with one row per setting and columns for the name and value of
#'   each setting.
#' @section YAML settings:
#'   The settings currently supported in `_targets.yaml` are as follows.
#'   * `store`: path to the data store of the pipeline. Usually,
#'     the data store lives at `_targets`. Set `store` to a custom directory
#'     to specify a path other than `_targets`. The path need not exist
#'     before the pipeline begins, and it need not end with "_targets",
#'     but it must be writeable.
#'     For optimal performance, read/write access should be fast.
#' @examples
#' if (identical(Sys.getenv("TAR_LONG_EXAMPLES"), "true")) {
#' tar_dir({ # tar_dir() runs code from a temporary directory.
#' tar_script(list(tar_target(x, 1 + 1)))
#' tar_yaml()
#' store_path <- tempfile()
#' line <- paste("store:", store_path)
#' writeLines(line, "_targets.yaml")
#' tar_yaml() # Should show "store" equal to a temporary file.
#' tar_make() # Writes to the custom data store path from _targets.yaml.
#' tar_read(x) # tar_read() knows about _targets.yaml too.
#' file.exists("_targets") # FALSE
#' file.exists(store_path) # TRUE
#' })
#' }
tar_yaml <- function() {
  tibble::tibble(name = "store", value = tar_object_yaml$store() %|||% NA_character_)
}
