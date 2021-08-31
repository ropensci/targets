# RStudio addins are tested interactively in
# tests/interactive/test-rstudio_addins.R. # nolint
# nocov start
#' @title RStudio addin to replace an assignment with the corresponding `"tar_target()"` at current selection.
#' @description For internal use only. Not a user-side function.
#' @export
#' @keywords internal
#' @param context RStudio API context from
#'   `rstudioapi::getActiveDocumentContext()`.
rstudio_addin_tar_assignment_to_target <- function() {
  tar_assert_package("rstudioapi")
  tar_assert_package("glue")
  tar_assert_package("stringr")
  sel <- rstudioapi::selectionGet()$value
  var_name <- stringr::str_extract(sel, "\\S+(?= ?<-)")
  body <- stringr::str_remove(sel, "\\S+ ?<- ?")
  body <- stringr::str_trim(body)
  body <- strsplit(body, "\n")[[1]]
  text <- glue::glue("tar_target(\n",
                     "  {var_name},\n",
                     "  {paste(body, collapse='\n  ')}\n",
                     ")", trim = FALSE)
  rstudioapi::insertText(text = text)
}

# nocov end
