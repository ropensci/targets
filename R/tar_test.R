#' @title Test code in a temporary directory.
#' @export
#' @description Runs a `test_that()` unit test inside a temporary
#'   directory to comply with CRAN policies. Also isolates `tar_option_set()`
#'   options and environment variables specific to `targets`.
#'   Useful for writing tests for
#'   [targetopia](https://wlandau.github.io/targetopia/) packages
#'   (extensions to `targets` tailored to specific use cases).
#' @return `NULL` (invisibly).
#' @param label Character of length 1, label for the test.
#' @param code User-defined code for the test.
#' @examples
#' tar_test("example test", {
#'   testing_variable_cafecfcb <- "only defined inside tar_test()"
#'   file.create("only_exists_in_tar_test")
#' })
#' exists("testing_variable_cafecfcb")
#' file.exists("only_exists_in_tar_test")
tar_test <- function(label, code) {
  assert_package("testthat")
  code <- substitute(code)
  expr <- substitute(
    tar_dir(testthat::test_that(label, code)),
    env = list(label = label, code = code)
  )
  withr::local_envvar(c(TAR_ASK = "false"))
  tar_option_reset()
  tar_option_set(envir = new.env(parent = globalenv()))
  suppressMessages(eval(expr, envir = parent.frame()))
  invisible()
}
