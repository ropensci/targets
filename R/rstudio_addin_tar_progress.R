#' @title RStudio addin to call tar_progress().
#' @description For internal use only. Not a user-side function.
#' @export
#' @keywords internal
rstudio_addin_tar_progress <- function() {
  assert_package("progress")
  View(tar_progress())
}
