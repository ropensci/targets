#' @title Set configuration settings.
#' @export
#' @family configuration
#' @description `tar_config_set()` writes special custom settings
#'   for the current project to an optional YAML configuration file.
#' @section Configuration:
#'   For several key functions like [tar_make()], the
#'   default values of arguments are controlled though
#'   `tar_config_get()`. `tar_config_get()` retrieves data
#'   from an optional YAML configuration file.
#'   You can control the settings in the YAML
#'   file programmatically with `tar_config_set()`.
#'   The default file path of this YAML file is `_targets.yaml`, and you can
#'   set another path globally using the `TAR_CONFIG`
#'   environment variable. The YAML file can store configuration
#'   settings for multiple projects, and you can globally
#'   set the default project with the `TAR_PROJECT` environment
#'   variable.
#'   The structure of the YAML file
#'   follows rules similar to the `config` R package, e.g.
#'   projects can inherit settings from one another using the `inherits` field.
#'   Exceptions include:
#'
#'   1. There is no requirement to have a configuration named `"default"`.
#'   2. Other projects do not inherit from the default project` automatically.
#'   3. Not all fields need values because `targets` already has defaults.
#'
#'   `targets` does not actually invoke
#'   the `config` package. The implementation in `targets`
#'   was written from scratch without viewing or copying any
#'   part of the source code of `config`.
#' @return `NULL` (invisibly)
#' @param inherits Character of length 1, name of the project from which
#'   the current project should inherit configuration settings.
#'   The current project is the `project` argument, which
#'   defaults to `Sys.getenv("TAR_PROJECT", "main")`.
#'   If the `inherits` argument `NULL`, the `inherits` setting is not modified.
#'   Use [tar_config_unset()] to delete a setting.
#' @param reporter_make Character of length 1, `reporter` argument to
#'   [tar_make()] and related functions that run the pipeline.
#'   If the argument `NULL`, the setting is not modified.
#'   Use [tar_config_unset()] to delete a setting.
#' @param reporter_outdated Character of length 1, `reporter` argument to
#'   [tar_outdated()] and related functions that do not run the pipeline.
#'   If the argument `NULL`, the setting is not modified.
#'   Use [tar_config_unset()] to delete a setting.
#' @param script Character of length 1, path to the target script file
#'   that defines the pipeline (`_targets.R` by default).
#'   This path should be either
#'   an absolute path or a path relative to the project root where you will
#'   call [tar_make()] and other functions. When [tar_make()] and friends
#'   run the script from the current working directory.
#'   If the argument `NULL`, the setting is not modified.
#'   Use [tar_config_unset()] to delete a setting.
#' @param shortcut logical of length 1, default `shortcut` argument
#'   to [tar_make()] and related functions.
#'   If the argument `NULL`, the setting is not modified.
#'   Use [tar_config_unset()] to delete a setting.
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
#'   If the argument `NULL`, the setting is not modified.
#'   Use [tar_config_unset()] to delete a setting.
#' @param workers Positive numeric of length 1, `workers` argument of
#'   [tar_make_clustermq()] and related functions that run the pipeline
#'   with parallel computing among targets.
#'   If the argument `NULL`, the setting is not modified.
#'   Use [tar_config_unset()] to delete a setting.
#' @param config Character of length 1, file path of the YAML
#'   configuration file with `targets` project settings.
#'   The `config` argument specifies which YAML configuration
#'   file that `tar_config_get()` reads from or `tar_config_set()`
#'   writes to in a single function call.
#'   It does not globally change which configuration file is used
#'   in subsequent function calls. The default file path of the YAML
#'   file is always `_targets.yaml` unless you set another
#'   default path using the `TAR_CONFIG` environment variable,
#'   e.g. `Sys.setenv(TAR_CONFIG = "custom.yaml")`. This also has the
#'   effect of temporarily modifying the default arguments to other functions
#'   such as [tar_make()] because the default arguments
#'   to those functions are controlled by `tar_config_get()`.
#' @param project Character of length 1, name of the current
#'   `targets` project. Thanks to the `config` R package,
#'   `targets` YAML configuration files can store multiple
#'   sets of configuration settings, with each set corresponding
#'   to its own project. The `project` argument allows you to
#'   set or get a configuration setting for a specific project
#'   for a given call to `tar_config_set()` or `tar_config_get()`.
#'   The default project is always called `"main"`
#'   unless you set another
#'   default project using the `TAR_PROJECT` environment variable,
#'   e.g. `Sys.setenv(tar_project = "custom")`. This also has the
#'   effect of temporarily modifying the default arguments to other functions
#'   such as [tar_make()] because the default arguments
#'   to those functions are controlled by `tar_config_get()`.
#' @examples
#' if (identical(Sys.getenv("TAR_EXAMPLES"), "true")) {
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
  inherits = NULL,
  reporter_make = NULL,
  reporter_outdated = NULL,
  store = NULL,
  shortcut = NULL,
  script = NULL,
  workers = NULL,
  config = Sys.getenv("TAR_CONFIG", "_targets.yaml"),
  project = Sys.getenv("TAR_PROJECT", "main")
) {
  # TODO: remove single-project format, which was deprecated on
  # 2021-09-03 (version 0.7.0.9001).
  tar_assert_chr(config)
  tar_assert_scalar(config)
  tar_assert_chr(project)
  tar_assert_scalar(project)
  tar_config_assert_inherits(inherits)
  tar_config_assert_reporter_make(reporter_make)
  tar_config_assert_reporter_outdated(reporter_outdated)
  tar_config_assert_script(script)
  tar_config_assert_shortcut(shortcut)
  tar_config_assert_store(store)
  tar_config_assert_workers(workers)
  yaml <- tar_config_read_yaml(config)
  if (!tar_config_is_multi_project(yaml, config)) {
    yaml <- tar_config_convert_multi_project(yaml, config)
  }
  yaml[[project]]$inherits <- inherits %|||% yaml[[project]]$inherits
  yaml[[project]]$reporter_make <- reporter_make %|||%
    yaml[[project]]$reporter_make
  yaml[[project]]$reporter_outdated <- reporter_outdated %|||%
    yaml[[project]]$reporter_outdated
  yaml[[project]]$script <- script %|||% yaml[[project]]$script
  yaml[[project]]$shortcut <- shortcut %|||% yaml[[project]]$shortcut
  yaml[[project]]$store <- store %|||% yaml[[project]]$store
  yaml[[project]]$workers <- if_any(
    is.null(workers),
    yaml[[project]]$workers,
    as.character(max(1L, workers))
  )
  dir_create(dirname(config))
  yaml::write_yaml(x = yaml, file = config)
  invisible()
}

tar_config_convert_multi_project <- function(yaml, config) {
  cli_blue_bullet(sprintf("Converting %s to multi-project format.", config))
  list(main = yaml)
}

tar_config_assert_inherits <- function(inherits) {
  if (is.null(inherits)) {
    return()
  }
  tar_assert_scalar(inherits)
  tar_assert_chr(inherits)
  tar_assert_nzchar(inherits)
}

tar_config_assert_reporter_make <- function(reporter_make) {
  if (is.null(reporter_make)) {
    return()
  }
  tar_assert_flag(reporter_make, tar_make_reporters())
}

tar_config_assert_reporter_outdated <- function(reporter_outdated) {
  if (is.null(reporter_outdated)) {
    return()
  }
  tar_assert_flag(reporter_outdated, tar_outdated_reporters())
}

tar_config_assert_script <- function(script) {
  if (is.null(script)) {
    return()
  }
  tar_assert_scalar(script)
  tar_assert_chr(script)
}

tar_config_assert_shortcut <- function(shortcut) {
  if (is.null(shortcut)) {
    return()
  }
  tar_assert_scalar(shortcut)
  tar_assert_lgl(shortcut)
}

tar_config_assert_store <- function(store) {
  if (is.null(store)) {
    return()
  }
  tar_assert_scalar(store)
  tar_assert_chr(store)
}

tar_config_assert_workers <- function(workers) {
  if (is.null(workers)) {
    return()
  }
  tar_assert_scalar(workers)
  tar_assert_dbl(workers)
  tar_assert_ge(workers, 1)
}

tar_make_reporters <- function() {
  c("verbose", "silent", "timestamp", "summary")
}

tar_outdated_reporters <- function() {
  c("forecast", "silent")
}

tar_config_read_yaml <- function(config) {
  if_any(file.exists(config), as.list(yaml::read_yaml(config)), list())
}
