tar_test <- function(label, code) {
  code <- substitute(code)
  expr <- substitute(
    tar_dir(testthat::test_that(label, code)),
    env = list(label = label, code = code)
  )
  withr::local_envvar(c(TAR_ASK = "false"))
  tar_option_reset()
  tar_option_set(envir = new.env(parent = globalenv()))
  suppressMessages(eval(expr, envir = parent.frame()))
}

expect_equiv <- function(object, expected, ...) {
  attributes(object) <- NULL
  attributes(expected) <- NULL
  expect_equal(object, expected, ...)
}
