tar_test <- function(label, code) {
  withr::local_envvar(c(TAR_SCRIPT_ASK = "false"))
  tar_option_reset()
  tar_option_set(envir = new.env(parent = globalenv()))
  tar_dir(testthat::test_that(label, code))
}
