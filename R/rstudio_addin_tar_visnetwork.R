# RStudio addins are tested interactively in
# tests/interactive/test-rstudio_addins.R. # nolint
# nocov start
#' @title RStudio addin to call [tar_visnetwork()].
#' @description For internal use only. Not a user-side function.
#' @export
#' @keywords internal
rstudio_addin_tar_visnetwork <- function() {
  print(tar_visnetwork())
}
# nocov end
