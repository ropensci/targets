tar_test <- function(label, code) {
  code <- substitute(code)
  expr <- substitute(
    testthat::test_that(label, code),
    env = list(label = label, code = code)
  )
  dir <- tempfile(pattern = "targets_")
  dir_create(dir)
  withr::local_envvar(c(TAR_ASK = "false"))
  withr::local_dir(dir)
  tar_option_reset()
  tar_option_set(envir = new.env(parent = globalenv()))
  eval(expr, envir = environment())
}
