# Start in a new R session.
library(proffer)

# Should barely see target_load_deps() in the profiling data.
# Use debug(store_read_path.tar_rds) to be sure readRDS() is not called. # nolint
tar_script({
  list(
    tar_target(x, seq_len(1e3)),
    tar_target(y, x, pattern = map(x))
  )
})
px <- pprof(tar_make(reporter = "summary", callr_function = NULL))

# Outdated targets
px <- pprof(tar_outdated(callr_function = NULL))

# Should see more time on target_ensure_deps() in the profiling data.
# Use debug(store_read_path.tar_rds) to be sure readRDS() is called. # nolint
tar_destroy()
tar_script({
  list(
    tar_target(x, seq_len(1e3), retrieval = "worker"),
    tar_target(y, x, pattern = map(x), retrieval = "worker")
  )
})
px <- pprof(tar_make(reporter = "summary", callr_function = NULL))

unlink("_targets.R")
tar_destroy()
