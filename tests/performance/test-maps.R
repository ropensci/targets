library(proffer)
library(targets)

# Should barely see target_load_deps() in the profiling data.
tar_script({
  tar_pipeline(
    tar_target(x, seq_len(1e3)),
    tar_target(y, x, pattern = map(x))
  )
})
px <- pprof(tar_make(reporter = "summary", callr_function = NULL))

# Outdated targets
px <- pprof(tar_outdated(callr_function = NULL))

# Should more time on target_load_deps() in the profiling data.
tar_destroy()
tar_script({
  tar_pipeline(
    tar_target(x, seq_len(1e3), retrieval = "remote"),
    tar_target(y, x, pattern = map(x), retrieval = "remote")
  )
})
px <- pprof(tar_make(reporter = "summary", callr_function = NULL))

unlink("_targets.R")
tar_destroy()
