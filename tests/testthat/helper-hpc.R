skip_hpc <- function() {
  condition <- identical(Sys.getenv("TARGETS_TEST_SKIP_HPC"), "true")
  message <- "slow hpc test"
  testthat::skip_if(condition = condition, message = message)
}
