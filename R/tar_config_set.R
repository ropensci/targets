#' @title Write configuration settings to _targets.yaml.
#' @export
#' @family configuration
#' @description `tar_config_set()` writes special custom settings
#'   to an optional project-level
#'   YAML configuration file (default: `_targets.yaml`).
#'   Most of these settings are default arguments shared across
#'   multiple functions called outside `_targets.R`.
#' @details Each project can have an optional YAML configuration file
#'   (default: `_targets.yaml` at the project root)
#'   with settings specific to a given project. You can either write it
#'   by hand or modify it with `tar_config_set()`, but `tar_config_set()`
#'   is recommended because it has guardrails to validate user input.
#'   The currently supported configuration settings are
#'   documented as the arguments of `tar_config_set()`.
#'
#'   `tar_config_set()` always writes a YAML file
#'   with a full set of configuration settings even when no
#'   arguments are supplied. To reset options completely,
#'   simply call `tar_config_set(config = "_targets.yaml")`
#'   and remove `_targets.yaml` if it exists.
#' @return `NULL` (invisibly)
#' @param reporter_make Character of length 1, `reporter` argument to
#'   [tar_make()] and related functions that run the pipeline.
#' @param reporter_outdated Character of length 1, `reporter` argument to
#'   [tar_outdated()] and related functions that do not run the pipeline.
#' @param config Character of length 1, path to the YAML file with
#'   all the configuration settings (default: `_targets.yaml`).
#' @param script Character of length 1, path to the target script file
#'   that defines the pipeline (`_targets.R` by default).
#'   This path should be either
#'   an absolute path or a path relative to the project root where you will
#'   call [tar_make()] and other functions. When [tar_make()] and friends
#'   run the script from the current working directory.
#' @param shortcut logical of length 1, default `shortcut` argument
#'   to [tar_make()] and related functions.
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
#' @param workers Positive numeric of length 1, `workers` argument of
#'   [tar_make_clustermq()] and related functions that run the pipeline
#'   with parallel computing among targets.
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
  config = "_targets.yaml",
  reporter_make = NULL,
  reporter_outdated = NULL,
  store = NULL,
  shortcut = NULL,
  script = NULL,
  workers = NULL
) {
  tar_config_assert_reporter_make(reporter_make)
  tar_config_assert_reporter_outdated(reporter_outdated)
  tar_config_assert_script(script)
  tar_config_assert_shortcut(shortcut)
  tar_config_assert_store(store)
  tar_config_assert_workers(workers)
  yaml <- tar_config_read_yaml(config)
  yaml$reporter_make <- reporter_make %|||% yaml$reporter_make
  yaml$reporter_outdated <- reporter_outdated %|||% yaml$reporter_outdated
  yaml$script <- script %|||% yaml$script
  yaml$shortcut <- shortcut %|||% yaml$shortcut
  yaml$store <- store %|||% yaml$store
  yaml$workers <- if_any(
    is.null(workers),
    yaml$workers,
    as.character(max(1L, workers))
  )
  dir_create(dirname(config))
  yaml::write_yaml(x = yaml, file = config)
  invisible()
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
