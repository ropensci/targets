# Start in a new R session.
library(proffer)

# Should barely see target_load_deps() in the profiling data.
# Use debug(store_read_path.tar_rds) to be sure readRDS() is not called. # nolint
tar_config_set(store = "_targets")
tar_script({
  options(clustermq.scheduler = "multicore")
  list(
    tar_target(x, seq_len(1e3)),
    tar_target(y, x, pattern = map(x))
  )
})
px <- pprof(tar_make(reporter = "summary", callr_function = NULL))

# With silent reporter
tar_destroy()
px <- pprof(tar_make(reporter = "silent", callr_function = NULL))

# With clustermq
tar_destroy()
px <- pprof(
  tar_make_clustermq(workers = 8, reporter = "silent", callr_function = NULL)
)

# With future
tar_destroy()
px <- pprof(tar_make_future(reporter = "silent", callr_function = NULL))

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

# Metadata deduplication is not a bottleneck.
tar_destroy()
tar_script({
  list(
    tar_target(x, seq_len(1e4)),
    tar_target(y, x, pattern = map(x))
  )
})
# Should and end start quickly:
tar_make(reporter = "summary", callr_function = NULL)
tar_script({
  list(
    tar_target(x, seq_len(1e4)),
    tar_target(y, 2 * x, pattern = map(x))
  )
})
# Should and end start quickly:
tar_make(reporter = "summary", callr_function = NULL)

# Clean up.
tar_destroy()
unlink("_targets.R")
unlink("_targets.yaml")
