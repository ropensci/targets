# RStudio addins are tested interactively in
# tests/interactive/test-rstudio_addins.R. # nolint
# nocov start
#' @title RStudio addin to call [tar_read()] on the symbol at the cursor.
#' @description For internal use only. Not a user-side function.
#' @export
#' @keywords internal
#' @param context RStudio API context from
#'   `rstudioapi::getActiveDocumentContext()`.
rstudio_addin_tar_read <- function(context = NULL) {
  assert_package("rstudioapi")
  context <- context %||% rstudioapi::getActiveDocumentContext()
  target <- rstudio_symbol_at_cursor(context)
  if (!is.null(target)) {
    cli_blue_bullet(paste("Loading target", target, "into global evironment."))
    env <- list(target = as.symbol(target))
    print(eval(substitute(targets::tar_read(target), env = env)))
  }
}
# nocov end
