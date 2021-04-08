#' @title Write a helper R script (raw version).
#' @export
#' @family scripts
#' @description Write a helper R script for a `targets` pipeline.
#'   Could be supporting functions or the `_targets.R` file itself.
#' @details `tar_helper_raw()` is a specialized version of [tar_script()]
#'   with flexible paths and tidy evaluation. It is like [tar_helper()]
#'   except that `code` is an "evaluated" argument rather than a quoted one.
#' @return `NULL` (invisibly)
#' @param code Expression object. `tar_helper_raw()` deparses and writes
#'   this code to a file at `path`, overwriting it if the file already exists.
#' @param path Character of length 1, path to write (or overwrite) `code`.
#'   If the parent directory does not exist, `tar_helper_raw()` creates it.
#' @examples
#' path <- tempfile()
#' tar_helper_raw(path, quote(x <- 1))
#' writeLines(readLines(path))
tar_helper_raw <- function(path = NULL, code = NULL) {
  assert_chr(path, "path must be a character.")
  assert_scalar(path, "path must have length 1.")
  dir_create(dirname(path))
  writeLines(deparse_script_code(code), path)
  invisible()
}
