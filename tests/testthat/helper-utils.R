tar_test <- function(label, code) {
  tar_dir(testthat::test_that(label, code))
}
