# This interactive test checks parallel efficiency.
# We should not have to wait for all the `x2` branches to finish
# before we move on to `x3`-`x6` branches downstream of the
# first x1 branches.
library(targets)
library(testthat)
tar_script({
  options(clustermq.scheduler = "multiprocess")
  list(
    tar_target(x1, c(0, 0, 5)),
    tar_target(x2, Sys.sleep(x1), pattern = map(x1)),
    tar_target(x3, x2, pattern = map(x2)),
    tar_target(x4, x3, pattern = map(x3)),
    tar_target(x5, x4, pattern = map(x4)),
    tar_target(x6, x5, pattern = map(x5))
  )
})
tar_make_clustermq(workers = 3L, reporter = "timestamp")
tar_destroy()

# All but 1 worker should quit while the last x4 branch is running.
# Run in a basic terminal, monitor with htop -d 1, and filter on R.home().
library(targets)
tar_script({
  options(clustermq.scheduler = "multiprocess")
  list(
    tar_target(x1, Sys.sleep(1)),
    tar_target(x2, list(Sys.sleep(1), x1)),
    tar_target(x3, list(Sys.sleep(0), x2)),
    tar_target(x4, list(Sys.sleep(5), x3))
  )
})
tar_make_clustermq(workers = 4L, reporter = "timestamp", callr_function = NULL)
tar_destroy()

# All but 1 worker should quit while the last x4 branch is running.
# Run in a basic terminal, monitor with htop -d 1, and filter on R.home().
library(targets)
tar_script({
  options(clustermq.scheduler = "multiprocess")
  sleep <- function(value, sleep) {
    Sys.sleep(sleep)
    value
  }
  list(
    tar_target(x1, c(0, 0, 0, 5)),
    tar_target(x2, sleep(x1, 5), pattern = map(x1)),
    tar_target(x3, sleep(x2, 5), pattern = map(x2)),
    tar_target(x4, sleep(x3, x3), pattern = map(x3))
  )
})
tar_make_clustermq(workers = 4L, reporter = "timestamp", callr_function = NULL)
expect_equal(tar_read(x4), c(0, 0, 0, 5))
expect_equal(tar_progress_branches()$built, c(4, 4, 4))
tar_destroy()

# All but 1 worker should quit while x3 is running,
# then all workers should quit while x4 is running.
# Run in a basic terminal, monitor with htop -d 1, and filter on R.home().
library(targets)
tar_script({
  options(clustermq.scheduler = "multiprocess")
  sleep <- function(value, sleep) {
    Sys.sleep(sleep)
    value
  }
  list(
    tar_target(x1, "pass"),
    tar_target(x2, sleep(x1, 5)),
    tar_target(x3, sleep(x2, 5)),
    tar_target(x4, sleep(x3, 5), deployment = "main")
  )
})
tar_make_clustermq(workers = 4L, reporter = "timestamp", callr_function = NULL)
tar_destroy()
unlink("_targets.R")

# Should not try to communicate with workers if they all
# shut down early: https://github.com/ropensci/targets/issues/404
tar_script({
  options(clustermq.scheduler = "multicore")
  tar_option_set(deployment = "main")
  list(
    tar_target(index, seq_len(4)),
    tar_target(run, index, pattern = map(index), deployment = "worker"),
    tar_target(end, run)
  )
})
tar_make_clustermq()
