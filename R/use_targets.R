#' @title Use targets
#' @export
#' @family help
#' @description Set up `targets` for an existing project.
#' @details `use_targets()` writes an example `_targets.R` script to
#'   get started with a `targets` pipeline for the current project.
#'   Follow the comments in this script to adapt it as needed.
#'   For more information, please visit
#'   <https://books.ropensci.org/targets/walkthrough.html>.
#' @return `NULL` (invisibly).
#' @inheritParams tar_script
#' @param open Logical of length 1, whether to open the file for editing
#'   in the RStudio IDE.
#' @param overwrite Logical of length 1, `TRUE` to overwrite the the target
#'   script file, `FALSE` otherwise.
#' @param scheduler Deprecated in version 1.5.0.9001 (2024-02-12).
#' @param job_name Deprecated in version 1.5.0.9001 (2024-02-12).
#' @examples
#' if (identical(Sys.getenv("TAR_INTERACTIVE_EXAMPLES"), "true")) {
#' tar_dir({ # tar_dir() runs code from a temp dir for CRAN.
#' use_targets(open = FALSE)
#' })
#' }
use_targets <- function(
  script = targets::tar_config_get("script"),
  open = interactive(),
  overwrite = FALSE,
  scheduler = NULL,
  job_name = NULL
) {
  tar_assert_scalar(script)
  tar_assert_chr(script)
  tar_assert_none_na(script)
  tar_assert_nzchar(script)
  tar_assert_lgl(open)
  tar_assert_scalar(open)
  tar_assert_none_na(open)
  tar_assert_lgl(overwrite)
  tar_assert_scalar(overwrite)
  tar_assert_none_na(overwrite)
  if (!is.null(scheduler)) {
    tar_warn_deprecate(
      "The 'scheduler' argument of use_targets() was deprecated ",
      "in version 1.5.0.9001 (2024-02-12) and is no longer used."
    )
  }
  if (!is.null(job_name)) {
    tar_warn_deprecate(
      "The 'job_name' argument of use_targets() was deprecated ",
      "in version 1.5.0.9001 (2024-02-12) and is no longer used."
    )
  }
  from <- file.path("pipelines", "use_targets.R")
  from <- system.file(from, package = "targets", mustWork = TRUE)
  if (overwrite || !file.exists(script)) {
    file.copy(from = from, to = script, overwrite = TRUE)
    cli_blue_play(paste("Wrote", script))
  } else {
    cli_red_x(paste("Did not overwrite", script))
  }
  # covered in tests/interactive/test-
  # nocov start
  if (open) {
    tar_assert_package("usethis")
    usethis::edit_file(path = script, open = TRUE)
  }
  # nocov end
  invisible()
}
