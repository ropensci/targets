#' @title Get configuration settings.
#' @export
#' @family configuration
#' @description Read the custom settings for the current project
#'   in the optional YAML configuration file.
#' @return The value of the configuration setting from
#'   the YAML configuration file (default: `_targets.yaml`)
#'   or the default value if the setting is not available.
#'   The data type of the return value depends on your choice
#'   of `name`.
#' @inheritSection tar_meta Storage access
#' @inheritSection tar_config_set Configuration
#' @inheritParams tar_config_set
#' @param name Character of length 1, name of the specific
#'   configuration setting to retrieve.
#' @examples
#' if (identical(Sys.getenv("TAR_EXAMPLES"), "true")) { # for CRAN
#' tar_dir({ # tar_dir() runs code from a temp dir for CRAN.
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
tar_config_get <- function(
  name,
  config = Sys.getenv("TAR_CONFIG", "_targets.yaml"),
  project = Sys.getenv("TAR_PROJECT", "main")
) {
  tar_assert_chr(config)
  tar_assert_scalar(config)
  tar_assert_chr(project)
  tar_assert_scalar(project)
  choices <- setdiff(names(formals(tar_config_set)), c("config", "project"))
  tar_assert_flag(name, choices = choices)
  yaml <- tar_config_yaml(config)
  value <- if_any(
    tar_config_is_multi_project(yaml, config),
    tar_config_get_multi_project(name, yaml, project, lookup_new()),
    tar_config_get_project(name, yaml)
  )
  tar_config_get_convert(name, value)
}

tar_config_is_multi_project <- function(yaml, config) {
  yaml <- yaml[!map_lgl(yaml, is.null)]
  out <- !length(yaml) || any(map_lgl(yaml, ~is.list(.x)))
  if (!out && any(file.exists(config))) {
    msg <- paste(
      "As of targets version 0.7.9001 (September 2021),",
      "targets YAML configuration files",
      "are moving to a format that supports multiple projects.",
      "Call tar_config_set(config = %s) to migrate",
      "your configuration file automatically.",
      "Read more at",
      "https://books.ropensci.org/targets/config.html"
    )
    tar_warn_deprecate(sprintf(msg, config))
  }
  out
}

tar_config_get_multi_project <- function(name, yaml, project, lookup) {
  value <- yaml[[project]][[name]]
  if (!is.null(value)) {
    return(value)
  }
  lookup_set(lookup, project, TRUE)
  inherits <- yaml[[project]]$inherits
  if (is.null(inherits)) {
    return(tar_config_get_project(name, list()))
  }
  if (lookup_exists(lookup, inherits)) {
    msg <- sprintf("Circular project inheritance: %s, %s", project, inherits)
    tar_throw_validate(msg)
  }
  tar_config_get_multi_project(name, yaml, inherits, lookup)
}

tar_config_get_project <- function(name, yaml) {
  switch(
    name,
    inherits = yaml$inherits,
    as_job = yaml$as_job %|||% FALSE,
    garbage_collection = yaml$garbage_collection,
    label = yaml$label %|||% character(0L),
    label_width = yaml$label_width %|||% 30L,
    level_separation = yaml$level_separation,
    reporter_make = yaml$reporter_make %|||% reporter_default(),
    reporter_outdated = yaml$reporter_outdated %|||% reporter_default(),
    script = yaml$script %|||% path_script_default(),
    seconds_meta_append = yaml$seconds_meta_append %|||% 0,
    seconds_meta_upload = yaml$seconds_meta_upload %|||% 15,
    seconds_reporter = yaml$seconds_reporter %|||% NULL,
    seconds_reporter_outdated = yaml$seconds_reporter_outdated %|||% NULL,
    seconds_interval = yaml$seconds_interval,
    shortcut = yaml$shortcut %|||% FALSE,
    store = yaml$store %|||% path_store_default(),
    use_crew = yaml$use_crew %|||% TRUE,
    workers = yaml$workers %|||% 1L
  )
}

tar_config_get_convert <- function(name, value) {
  switch(
    name,
    inherits = if_any(is.null(value), NULL, as.character(value)),
    as_job = as.logical(value),
    garbage_collection = if_any(is.null(value), NULL, as.logical(value)),
    label = as.character(value),
    label_width = if_any(is.null(value), NULL, as.integer(value)),
    level_separation = if_any(is.null(value), NULL, as.numeric(value)),
    reporter_make = as.character(value),
    reporter_outdated = as.character(value),
    script = as.character(value),
    seconds_meta_append = as.numeric(value),
    seconds_meta_upload = as.numeric(value),
    seconds_reporter = if_any(is.null(value), NULL, as.numeric(value)),
    seconds_reporter_outdated = if_any(
      is.null(value),
      NULL,
      as.numeric(value)
    ),
    seconds_interval = if_any(is.null(value), NULL, as.numeric(value)),
    shortcut = as.logical(value),
    store = as.character(value),
    use_crew = as.logical(value),
    workers = as.integer(max(1L, as.integer(value)))
  )
}

reporter_default <- function() {
  if_any(
    isTRUE(getOption("knitr.in.progress")) || !interactive(),
    "terse",
    "balanced"
  )
}
