# RStudio addins are tested interactively in
# tests/interactive/test-rstudio_addins.R.
# nocov start
#' @title RStudio addin to call tar_outdated().
#' @description For internal use only. Not a user-side function.
#' @export
#' @keywords internal
rstudio_addin_tar_outdated <- function() {
  assert_package("outdated")
  print(tar_outdated())
}
# nocov end
