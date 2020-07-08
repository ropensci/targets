tar_test <- function(label, code) {
  withr::local_options(list(tar_script_ask = FALSE))
  tar_dir(testthat::test_that(label, code))
}
