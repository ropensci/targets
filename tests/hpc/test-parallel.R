# Targets x1 and x2 should run on parallel workers.
# Run in a basic terminal, monitor with htop -d 1, and filter on R.home().
library(targets)
tar_script({
  tar_option_set(controller = crew::crew_controller_local(workers = 2L))
  list(
    tar_target(x1, list(Sys.sleep(5), 1)),
    tar_target(x2, list(Sys.sleep(5), 2))
  )
})
tar_make()
tar_destroy()

# This interactive test checks parallel efficiency.
# We should not have to wait for all the `x2` branches to finish
# before we move on to `x3`-`x6` branches downstream of the
# first x1 branches.
library(targets)
library(testthat)
tar_script({
  tar_option_set(controller = crew::crew_controller_local(workers = 3L))
  list(
    tar_target(x1, c(0, 0, 5)),
    tar_target(x2, Sys.sleep(x1), pattern = map(x1)),
    tar_target(x3, x2, pattern = map(x2)),
    tar_target(x4, x3, pattern = map(x3)),
    tar_target(x5, x4, pattern = map(x4)),
    tar_target(x6, x5, pattern = map(x5))
  )
})
tar_make(reporter = "timestamp")
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
expect_equal(unname(tar_read(x4)), c(0, 0, 0, 5))
expect_equal(tar_progress_branches()$completed, c(4, 4, 4))
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
  options(clustermq.scheduler = "multiprocess")
  tar_option_set(deployment = "main")
  list(
    tar_target(index, seq_len(4)),
    tar_target(run, index, pattern = map(index), deployment = "worker"),
    tar_target(end, run)
  )
})
tar_make_clustermq()
tar_destroy()
unlink("_targets.R")

# error = "abridge" should keep current targets going.
# Workers should clean up.
tar_script({
  tar_option_set(controller = crew::crew_controller_local(workers = 3L))
  error_middle <- function() {
    Sys.sleep(2)
    stop("time up")
  }
  just_sleep_short <- function() {
    Sys.sleep(4)
  }
  just_sleep_long <- function() {
    Sys.sleep(5)
  }
  list(
    tar_target(w, error_middle(), error = "abridge"),
    tar_target(x, just_sleep_short()),
    tar_target(y, just_sleep_long()),
    tar_target(z, list(w, x, y))
  )
})
tar_make()
out <- tar_progress()
expect_equal(nrow(out), 3L)
expect_equal(out$progress[out$name == "w"], "errored")
expect_equal(out$progress[out$name == "x"], "completed")
expect_equal(out$progress[out$name == "y"], "completed")
expect_false("z" %in% out$name)
tar_destroy()
unlink("_targets.R")
