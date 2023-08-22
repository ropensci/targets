#' @title Use targets with Target Markdown.
#' @export
#' @family help
#' @description Create an example Target Markdown report
#'   to get started with {targets}.
#' @return `NULL` (invisibly).
#' @param path Character of length 1, output path of the
#'   Target Markdown report relative to the current active
#'   project.
#' @param open Logical, whether to open the file for editing
#'   in the RStudio IDE.
#' @examples
#' if (identical(Sys.getenv("TAR_INTERACTIVE_EXAMPLES"), "true")) {
#' tar_dir({ # tar_dir() runs code from a temp dir for CRAN.
#' use_targets(open = FALSE)
#' })
#' }
use_targets_rmd <- function(path = "_targets.Rmd", open = interactive()) {
  # Covered in tests/interactive/test-use_targets_rmd.R.
  # nocov start
  tar_assert_allow_meta("use_targets_rmd")
  tar_assert_package("usethis")
  source <- file.path(
    "..",
    "rmarkdown",
    "templates",
    "targets",
    "skeleton",
    "skeleton.Rmd"
  )
  usethis::use_template(
    template = source,
    save_as = path,
    package = "targets",
    open = FALSE
  )
  if (identical(open, TRUE)) {
    usethis::edit_file(path = path, open = open)
  }
  invisible()
  # nocov end
}
