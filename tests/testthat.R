Sys.setenv(PROCESSX_NOTIFY_OLD_SIGCHLD = "true")
library(testthat)
library(targets)
test_check("targets", reporter = ProgressReporter$new())
