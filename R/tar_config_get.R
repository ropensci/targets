#' @title Get configuration settings from _targets.yaml.
#' @export
#' @family configuration
#' @description Read the custom settings in the optional _targets.yaml
#'   configuration file at the current project.
#' @details `_targets.yaml` is an optional YAML configuration file
#'   with settings specific to a given project. You can write it
#'   by hand or modify it with [tar_config_set()].
#'   In order to work properly, `_targets.yaml` must live at the
#'   root of the project (next to `_targets.R`).
#'   The currently supported configuration settings are
#'   documented as the arguments of [tar_config_set()].
#' @return The value of the configuration setting from `_targets.yaml`,
#'   or the default value if the setting is not available.
#'   The data type of the return value depends on your choice
#'   of `name`.
#' @param name Character of length 1, name of the specific
#'   `_targets.yaml` configuration setting to retrieve.
#' @examples
#' if (identical(Sys.getenv("TAR_LONG_EXAMPLES"), "true")) {
#' tar_dir({ # tar_dir() runs code from a temporary directory.
#' tar_script(list(tar_target(x, 1 + 1)))
#' tar_config_get("store") # "_targets"
#' store_path <- tempfile()
#' tar_config_set(store = store_path)
#' tar_config_get("store") # Shows a temp file.
#' tar_make() # Writes to the custom data store identified in _targets.yaml.
#' tar_read(x) # tar_read() knows about _targets.yaml too.
#' file.exists("_targets") # FALSE
#' file.exists(store_path) # TRUE
#' })
#' }
tar_config_get <- function(name) {
  assert_flag(name, choices = names(formals(tar_config_set)))
  switch(
    name,
    store = tar_config$get_value("store") %|||% path_store_default()
  )
}
