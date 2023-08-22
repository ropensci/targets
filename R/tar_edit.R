#' @title Open the target script file for editing.
#' @export
#' @family scripts
#' @description Open the target script file for editing.
#'   Requires the `usethis` package.
#' @details The target script file is an R code file
#'   that defines the pipeline. The default path is `_targets.R`,
#'   but the default for the current project
#'   can be configured with [tar_config_set()]
#' @inheritParams tar_validate
tar_edit <- function(script = targets::tar_config_get("script")) {
  # Covered in tests/interactive/test-tar_edit.R # nolint
  # nocov start
  tar_assert_allow_meta("tar_edit")
  tar_assert_package("usethis")
  usethis::edit_file(path = script, open = TRUE)
  # nocov end
}
