#' @title Write a helper R script.
#' @export
#' @family scripts
#' @description Write a helper R script for a `targets` pipeline.
#'   Could be supporting functions or the target script file
#'   (default: `_targets.R`) itself.
#' @details `tar_helper()` is a specialized version of [tar_script()]
#'   with flexible paths and tidy evaluation.
#' @return `NULL` (invisibly)
#' @param code Quoted code to write to `path`.
#'   `tar_helper()` overwrites the file if it already exists.
#' @param path Character of length 1, path to write (or overwrite) `code`.
#'   If the parent directory does not exist, `tar_helper_raw()` creates it.
#' @param tidy_eval Logical, whether to use tidy evaluation on `code`. If
#'   turned on, you can substitute expressions and symbols using `!!` and `!!!`.
#'   See examples below.
#' @param envir Environment for tidy evaluation.
#' @examples
#' # Without tidy evaluation:
#' path <- tempfile()
#' tar_helper(path, x <- 1)
#' writeLines(readLines(path))
#' # With tidy evaluation:
#' y <- 123
#' tar_helper(path, x <- !!y)
#' writeLines(readLines(path))
tar_helper <- function(
  path = NULL,
  code = NULL,
  tidy_eval = TRUE,
  envir = parent.frame()
) {
  force(envir)
  tar_assert_lgl(tidy_eval)
  tar_assert_scalar(tidy_eval)
  tar_assert_envir(envir)
  tar_helper_raw(path, tar_tidy_eval(substitute(code), envir, tidy_eval))
}
