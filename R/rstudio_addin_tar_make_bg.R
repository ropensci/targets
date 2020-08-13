# RStudio addins are tested interactively in
# tests/interactive/test-rstudio_addins.R.
# nocov start
#' @title RStudio addin to call tar_progress().
#' @description For internal use only. Not a user-side function.
#' @export
#' @keywords internal
rstudio_addin_tar_make_bg <- function() {
  tar_make(callr_function = callr::r_bg)
}
# nocov end
