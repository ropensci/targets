# This interactive test checks parallel efficiency.
# We should not have to wait for all the `x2` branches to finish
# before we move on to `x3`-`x6` branches downstream of the
# first x1 branches.
library(targets)
tar_script({
  options(clustermq.scheduler = "multicore")
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
