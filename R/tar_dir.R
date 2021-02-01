#' @title Execute code in a temporary directory.
#' @export
#' @description Runs code inside a new `tempfile()` directory
#'   in order to avoid writing to the user's file space.
#'   Used in examples and tests in order to comply with CRAN policies.
#' @return Return value of the user-defined code.
#' @param code User-defined code.
#' @examples
#' tar_dir(file.create("only_exists_in_tar_dir"))
#' file.exists("only_exists_in_tar_dir")
tar_dir <- function(code) {
  code <- substitute(code)
  dir <- tempfile(pattern = "targets_")
  dir_create(dir)
  withr::local_dir(dir)
  eval(code, envir = parent.frame())
}
