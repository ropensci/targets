#' @title Write configuration settings to _targets.yaml.
#' @export
#' @family configuration
#' @description `tar_config_set()` writes special custom settings
#'   to an optional project-level
#'   YAML configuration file (default: `_targets.yaml`).
#'   Only the non-`NULL` config settings
#'   are actually set. Do not invoke while the pipeline is running.
#' @details Each project can have an optional YAML configuration file
#'   (default: `_targets.yaml` at the project root)
#'   with settings specific to a given project. You can write it
#'   by hand or modify it with `tar_config_set()`.
#'   The currently supported configuration settings are
#'   documented as the arguments of `tar_config_set()`.
#'
#'   `tar_config_set()` always writes a YAML file
#'   with a full set of configuration settings even when no
#'   arguments are supplied. To reset options completely,
#'   simply call `tar_config_set(config = "_targets.yaml")`
#'   and remove `_targets.yaml` if it exists.
#' @return `NULL` (invisibly)
#' @param script Character of length 1, path to the target script file
#'   that defines the pipeline (`_targets.R` by default).
#'   This path should be either
#'   an absolute path or a path relative to the project root where you will
#'   call [tar_make()] and other functions. When [tar_make()] and friends
#'   [source()] the script, they do not change the working directory
#'   (i.e. the default `source(chdir = FALSE)`).
#' @param store Character of length 1, path to the data store of the pipeline.
#'   If `NULL`, the `store` setting is left unchanged in the
#'   YAML configuration file (default: `_targets.yaml`).
#'   Usually, the data store lives at `_targets`.
#'   Set `store` to a custom directory
#'   to specify a path other than `_targets/`. The path need not exist
#'   before the pipeline begins, and it need not end with "_targets",
#'   but it must be writeable.
#'   For optimal performance, choose a storage location
#'   with fast read/write access.
#' @param config Character of length 1, path to the YAML file with
#'   all the configuration settings (default: `_targets.yaml`).
#' @examples
#' if (identical(Sys.getenv("TAR_LONG_EXAMPLES"), "true")) {
#' tar_dir({ # tar_dir() runs code from a temporary directory.
#' tar_script(list(tar_target(x, 1 + 1)))
#' tar_config_get("store") # NULL (data store defaults to "_targets/")
#' store_path <- tempfile()
#' tar_config_set(store = store_path)
#' tar_config_get("store") # Shows a temp file.
#' tar_make() # Writes to the custom data store identified in _targets.yaml.
#' tar_read(x) # tar_read() knows about _targets.yaml too.
#' file.exists("_targets") # FALSE
#' file.exists(store_path) # TRUE
#' })
#' }
tar_config_set <- function(
  store = NULL,
  script = NULL,
  config = "_targets.yaml"
) {
  yaml <- tar_config_read_yaml(config)
  yaml$script <- script %|||% yaml$script
  yaml$store <- store %|||% yaml$store
  dir_create(dirname(config))
  yaml::write_yaml(x = yaml, file = config)
  invisible()
}
