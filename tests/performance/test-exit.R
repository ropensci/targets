# Test workaround of https://github.com/r-lib/callr/issues/185
library(targets)
tar_destroy()
tar_script({
  library(targets)
  list(
    tar_target(x, runif(1e7)),
    tar_target(y, stop(x[1]))
  )
})

# Should exit fast after y errors out:
tar_make(reporter = "timestamp")

tar_script({
  library(targets)
  tar_option_set(error = "workspace")
  list(
    tar_target(x, runif(1e7)),
    tar_target(y, stop(x[1]))
  )
})

# Should exit fast after y errors out:
tar_make(reporter = "timestamp")

tar_script({
  library(targets)
  tar_option_set(error = "continue")
  list(
    tar_target(x, runif(1e7)),
    tar_target(y, stop(x[1]))
  )
})

# Should run fast:
tar_make(reporter = "timestamp")

tar_destroy()
unlink("_targets.R")
