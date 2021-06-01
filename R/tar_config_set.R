#' @title Write configuration settings to _targets.yaml.
#' @export
#' @family configuration
#' @description `tar_config_set()` writes special custom settings
#'   to an optional `_targets.yaml` configuration file at the
#'   current project. Only the non-`NULL` config settings
#'   are actually set. Do not invoke while the pipeline is running.
#' @details `_targets.yaml` is an optional YAML configuration file
#'   with settings specific to a given project. You can write it
#'   by hand or modify it with `tar_config_set()`.
#'   In order to work properly, `_targets.yaml` must live at the
#'   root of the project.
#'   The currently supported configuration settings are
#'   documented as the arguments of `tar_config_set()`.
#'
#'   `tar_config_set()` always writes a `_targets.yaml` file
#'   with a full set of configuration settings even when no
#'   arguments are supplied. To reset options completely,
#'   simply remove `_targets.yaml`.
#' @return `NULL` (invisibly)
#' @param script Character of length 1, path to the target script file
#'   that defines the pipeline (`_targets.R` by default).
#'   This path should be either
#'   an absolute path or a path relative to the project root where you will
#'   call [tar_make()] and other functions. When [tar_make()] and friends
#'   [source()] the script, they do not change the working directory
#'   (i.e. the default `source(chdir = FALSE)`).
#' @param store Character of length 1, path to the data store of the pipeline.
#'   If `NULL`, the `store` setting is left unchanged in `_targets.yaml`.
#'   Usually, the data store lives at `_targets`.
#'   Set `store` to a custom directory
#'   to specify a path other than `_targets/`. The path need not exist
#'   before the pipeline begins, and it need not end with "_targets",
#'   but it must be writeable.
#'   For optimal performance, choose a storage location
#'   with fast read/write access.
#' @param path Character of length 1, path to the YAML file with
#'   all the configuration settings (default: `_targets.yaml`).
#'   Only applies to the current R session,
#'   the path reverts back to `_targets.yaml` when you restart R.
#'   Intended for niche use cases only, such as `tar_watch()`
#'   embedded as a module in Shiny apps that use `targets`.
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
tar_config_set <- function(store = NULL, script = NULL, path = NULL) {
  if_any(is.null(path), NULL, tar_config$set_path(path))
  if_any(is.null(script), NULL, tar_config$set_script(script))
  if_any(is.null(store), NULL, tar_config$set_store(store))
  invisible()
}
