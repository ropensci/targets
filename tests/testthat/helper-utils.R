tar_test <- function(label, code) {
  withr::local_envvar(c(TAR_SCRIPT_ASK = "false"))
  tar_option_reset()
  tar_options(envir = new.env(parent = globalenv()))
  tar_dir(testthat::test_that(label, code))
}
