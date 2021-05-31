#' @title Check if the `targets` script file exists.
#' @export
#' @family existence
#' @description Check if the `targets` script file exists for the
#'   current project. The `targets` script is `_targets.R` by default,
#'   but the path can be configured for the current project
#'   using [tar_config_set()].
#' @return Logical of length 1, whether the current project's metadata exists.
#' @examples
#' tar_exist_script()
tar_exist_script <- function() {
  file.exists(path_script())
}
