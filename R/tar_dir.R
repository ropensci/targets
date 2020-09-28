#' @title Execute code in a temporary directory.
#' @export
#' @keywords internal
#' @description Creates a new directory at `tempfile()` and runs code
#'   inside with `withr::with_dir()`
#' @return Return value of the user-defined code.
#' @param code User-defined code.
#' @examples
#' tar_dir(file.create("x"))
#' file.exists("x") # Should be false.
tar_dir <- function(code) {
  code <- substitute(code)
  dir <- tempfile(pattern = "targets_")
  dir_create(dir)
  withr::local_dir(dir)
  eval(code, envir = parent.frame())
}
