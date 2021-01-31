#' @title Open _targets.R for editing.
#' @export
#' @description Looks for _targets.R in the current working directory.
#'   Requires the `usethis` package.
tar_edit <- function() {
  # Covered in tests/interactive/test-tar_edit.R # nolint
  # nocov start
  assert_package("usethis")
  usethis::edit_file(path = path_script(), open = TRUE)
  # nocov end
}
