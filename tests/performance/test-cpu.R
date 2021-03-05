# Restart R and monitor CPU usage.
rstudioapi::restartSession()
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
