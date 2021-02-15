#' @title Check if the target script exists.
#' @export
#' @description Check if the `_targets.R` file of the current project exists.
#'   exists for the current project.
#' @return Logical of length 1, whether the current project's metadata exists.
#' @examples
#' tar_exist_script()
tar_exist_script <- function() {
  file.exists(path_script())
}
