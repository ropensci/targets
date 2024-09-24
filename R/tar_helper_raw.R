#' @rdname tar_helper
#' @export
tar_helper_raw <- function(path = NULL, code = NULL) {
  tar_assert_chr(path)
  tar_assert_scalar(path)
  dir_create(dirname(path))
  writeLines(deparse_script_code(code), path)
  invisible()
}
