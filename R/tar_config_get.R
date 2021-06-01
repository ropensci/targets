#' @title Get configuration settings from _targets.yaml.
#' @export
#' @family configuration
#' @description Read the custom settings in the optional _targets.yaml
#'   configuration file at the current project.
#' @details Each project can have an optional YAML configuration file
#'   (default: `_targets.yaml`) with settings specific to a given project.
#'   You can write it by hand or modify it with [tar_config_set()].
#'   The currently supported configuration settings are
#'   documented as the arguments of [tar_config_set()].
#' @return The value of the configuration setting from
#'   the YAML configuration file (default: `_targets.yaml`)
#'   or the default value if the setting is not available.
#'   The data type of the return value depends on your choice
#'   of `name`.
#' @param name Character of length 1, name of the specific
#'   configuration setting to retrieve. If `name` is `"config"`,
#'   then instead of retrieving a specific setting,
#'   then `tar_config_get()` will return the path to the YAML
#'   file where those settings are stored for the
#'   current R session (default: `_targets.yaml`).
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
    script = tar_config$get_script(),
    store = tar_config$get_store(),
    config = tar_config$get_path()
  )
}
