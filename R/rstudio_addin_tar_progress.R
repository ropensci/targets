# RStudio addins are tested interactively in
# tests/interactive/test-rstudio_addins.R.
# nocov start
#' @title RStudio addin to call tail(tar_progress()).
#' @description For internal use only. Not a user-side function.
#' @export
#' @keywords internal
rstudio_addin_tar_progress <- function() {
  utils::tail(tar_progress(), n = 20)
}
# nocov end
