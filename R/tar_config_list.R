#' @title List projects.
#' @export
#' @family configuration
#' @description List the names of projects defined in `_targets.yaml`.
#' @return Character vector of names of projects defined in `_targets.yaml`.
#' @inheritSection tar_meta Storage access
#' @inheritSection tar_config_set Configuration
#' @inheritParams tar_config_set
#' @examples
#' yaml <- tempfile()
#' tar_config_set(store = "my_store_a", config = yaml, project = "project_a")
#' tar_config_set(store = "my_store_b", config = yaml, project = "project_b")
#' tar_config_projects(config = yaml)
tar_config_projects <- function(
  config = Sys.getenv("TAR_CONFIG", "_targets.yaml")
) {
  names(tar_config_yaml(config = config))
}
