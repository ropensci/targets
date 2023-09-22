#' @title Read `_targets.yaml`.
#' @export
#' @family configuration
#' @description Read the YAML content of `_targets.yaml`.
#' @return Nested list of fields defined in `_targets.yaml`.
#' @inheritSection tar_meta Storage access
#' @inheritSection tar_config_set Configuration
#' @inheritParams tar_config_set
#' @examples
#' yaml <- tempfile()
#' tar_config_set(store = "my_store_a", config = yaml, project = "project_a")
#' tar_config_set(store = "my_store_b", config = yaml, project = "project_b")
#' str(tar_config_yaml(config = yaml))
tar_config_yaml <- function(
  config = Sys.getenv("TAR_CONFIG", "_targets.yaml")
) {
  tar_assert_chr(config)
  tar_assert_scalar(config)
  yaml <- tar_config_read_yaml(config)
  if_any(
    tar_config_is_multi_project(yaml, config),
    yaml,
    tar_config_convert_multi_project(yaml, config)
  )
}
