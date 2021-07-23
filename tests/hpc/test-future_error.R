library(targets)
tar_script({
  future::plan(future.callr::callr)
  tar_target(x, Sys.sleep(400), error = "continue")
})
tar_make_future()
# Terminate the worker. Should show a callr error message:
tar_meta(x, error)$error
