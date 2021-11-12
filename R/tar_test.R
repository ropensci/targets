#' @title Test code in a temporary directory.
#' @export
#' @family utilities to extend targets
#' @description Runs a `test_that()` unit test inside a temporary
#'   directory to avoid writing to the user's file space.
#'   This helps ensure compliance with CRAN policies.
#'   Also isolates `tar_option_set()`
#'   options and environment variables specific to `targets`
#'   and skips the test on Solaris.
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
  platform <- tolower(Sys.info()[["sysname"]])
  if (platform != "solaris") {
    tar_assert_package("testthat")
    code <- substitute(code)
    expr <- substitute(
      targets::tar_dir(testthat::test_that(label, code)),
      env = list(label = label, code = code)
    )
    withr::local_envvar(c(TAR_ASK = "false", TAR_TEST = "true"))
    tar_option_reset()
    on.exit(tar_option_reset(), add = TRUE)
    tar_option_set(envir = new.env(parent = globalenv()))
    suppressMessages(eval(expr, envir = parent.frame()))
  }
  invisible()
}
