# RStudio addins are tested interactively in
# tests/interactive/test-rstudio_addins.R. # nolint
# nocov start
#' @title RStudio addin to print `tail(tar_progress())`.
#' @description For internal use only. Not a user-side function.
#' @export
#' @keywords internal
rstudio_addin_tar_progress <- function() {
  print(utils::tail(tar_progress(), n = 20))
}
# nocov end
