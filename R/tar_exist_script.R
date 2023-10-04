#' @title Check if the target script file exists.
#' @export
#' @family existence
#' @description Check if the target script file exists for the
#'   current project. The target script is `_targets.R` by default,
#'   but the path can be configured for the current project
#'   using [tar_config_set()].
#' @return Logical of length 1, whether the current project's metadata exists.
#' @inheritParams tar_validate
#' @examples
#' tar_exist_script()
tar_exist_script <- function(script = targets::tar_config_get("script")) {
  file.exists(script)
}
