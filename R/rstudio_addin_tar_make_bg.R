# RStudio addins are tested interactively in
# tests/interactive/test-rstudio_addins.R. # nolint
# nocov start
#' @title RStudio addin to run [tar_make()] in the background.
#' @description For internal use only. Not a user-side function.
#' @export
#' @keywords internal
rstudio_addin_tar_make_bg <- function() {
  tar_make(callr_function = callr::r_bg)
  cli::cli_alert(
    "Running tar_make() in background. Monitor with tar_progress()."
  )
}
# nocov end
