library(targets)
tar_script({
  future::plan(future.callr::callr)
  list(
    tar_target(x, Sys.sleep(400), error = "continue"),
    tar_target(y, x)
  )
})
tar_make_future()
# Terminate the worker.
# Target y should start and target x should show a callr error message:
tar_meta(x, error)$error
