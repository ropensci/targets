#' @title Open the `targets` script file.
#' @export
#' @family scripts
#' @description Open the `targets` script file for editing.
#'   Requires the `usethis` package.
#' @details The `targets` script file is an R code file
#'   that defines the pipeline. The default path is `_targets.R`,
#'   but the default for the current project
#'   can be configured with [tar_config_set()].
tar_edit <- function() {
  # Covered in tests/interactive/test-tar_edit.R # nolint
  # nocov start
  assert_package("usethis")
  usethis::edit_file(path = path_script(), open = TRUE)
  # nocov end
}
