# Restart R.
rstudioapi::restartSession()

# Set up pipeline.
library(targets)
tar_script({
  library(targets)
  message("starting psock")
  future::plan(future::multisession)
  list(tar_target(x, Sys.sleep(360)))
})

# CPU usage should drop down to almost nothing
# as exponential backoff kicks in.
tar_make_future(callr_function = NULL)

# Restart R.
rstudioapi::restartSession()

# Set up pipeline.
library(targets)
tar_script({
  library(targets)
  options(clustermq.scheduler = "multiprocess")
  list(tar_target(x, Sys.sleep(360)))
})

# CPU usage should drop down to almost nothing.
tar_make_clustermq(callr_function = NULL)
