tar_test <- function(label, code) {
  withr::local_envvar(c(TAR_SCRIPT_ASK = "false"))
  tar_dir(testthat::test_that(label, code))
}
