#' @title Unset configuration settings.
#' @export
#' @family configuration
#' @description Unset (i.e. delete) one or more
#'   custom settings for the current project
#'   from the optional YAML configuration file.
#'   After that, [tar_option_get()] will return the original
#'   default values for those settings for the project.
#' @return `NULL` (invisibly)
#' @inheritSection tar_config_set Configuration
#' @inheritParams tar_config_set
#' @param names Character vector of configuration settings
#'   to delete from the current project.
#' @examples
#' if (identical(Sys.getenv("TAR_EXAMPLES"), "true")) {
#' tar_dir({ # tar_dir() runs code from a temporary directory.
#' tar_script(list(tar_target(x, 1 + 1)))
#' tar_config_get("store") # "_targets"
#' store_path <- tempfile()
#' tar_config_set(store = store_path)
#' tar_config_get("store") # Shows a temp file.
#' tar_config_unset("store")
#' tar_config_get("store") # _targets
#' })
#' }
tar_config_unset <- function(
  names = character(0),
  config = Sys.getenv("TAR_CONFIG", "_targets.yaml"),
  project = Sys.getenv("TAR_PROJECT", "main")
) {
  tar_assert_chr(config)
  tar_assert_scalar(config)
  tar_assert_chr(project)
  tar_assert_scalar(project)
  choices <- setdiff(names(formals(tar_config_set)), c("config", "project"))
  tar_assert_in(names, choices = choices)
  yaml <- tar_config_read_yaml(config)
  if (!tar_config_is_multi_project(yaml, config)) {
    yaml <- tar_config_convert_multi_project(yaml, config)
  }
  for (name in names) {
    yaml[[project]][[name]] <- NULL
  }
  dir_create(dirname(config))
  yaml::write_yaml(x = yaml, file = config)
  invisible()
}
