# RStudio addins are tested interactively in
# tests/interactive/test-rstudio_addins.R. # nolint
# nocov start
#' @title RStudio addin to insert `"tar_target()"` at the cursor.
#' @description For internal use only. Not a user-side function.
#' @export
#' @keywords internal
#' @param context RStudio API context from
#'   `rstudioapi::getActiveDocumentContext()`.
rstudio_addin_tar_target <- function(context = NULL) {
  assert_package("rstudioapi")
  context <- context %|||% rstudioapi::getActiveDocumentContext()
  rstudioapi::insertText(text = "tar_target()", id = context$id)
}
# nocov end
