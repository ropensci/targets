#' @title RStudio addin to call tar_visnetwork().
#' @description For internal use only. Not a user-side function.
#' @export
#' @keywords internal
rstudio_addin_tar_visnetwork <- function() {
  assert_package("visNetwork")
  print(tar_visnetwork())
}
