library(proffer)
library(targets)

px <- pprof(
  targets <- lapply(
    as.character(seq_len(1e3)), function(name) {
      target_init(name, expr = quote(1 + 1))
    }
  )
)
large_pipeline <- pipeline_init(targets)
px <- pprof(algorithm_init("local", pipeline = large_pipeline)$run())
tar_destroy()

# With API
tar_script({
  targets <- lapply(
    as.character(seq_len(1e3)), function(name) {
      tar_target_external(name, expr = quote(1 + 1))
    }
  )
  tar_pipeline(targets)
})
px <- pprof(tar_make(reporter = "summary", callr_function = NULL))
tar_destroy()
unlink("_targets.R")
