# Test that data storage is a bottleneck.
library(proffer)
tar_destroy()
tar_option_set(memory = "transient")
targets <- lapply(
  seq_len(50),
  function(index) {
    tar_target_raw(
      as.character(index),
      quote(data.frame(x = runif(1e7))),
      memory = "transient",
      format = "fst"
    )
  }
)
pipeline <- pipeline_init(targets)
local <- local_init(pipeline)
px <- pprof(local$run())
tar_destroy()
