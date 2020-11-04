library(targets)
library(pryr)

tar_script({
  # Comment out and see memory increase:
  tar_option_set(memory = "transient", garbage_collection = TRUE)
  targets <- lapply(
    seq_len(50),
    function(index) {
      tar_target_raw(
        as.character(index),
        quote({
          print(pryr::mem_used())
          runif(1e6) # 8 MB memory
        })
      )
    }
  )
  tar_pipeline(targets)
})

# Memory usage should stay constant over time.
tar_destroy()
tar_make()

# The final size of the pipeline object should be small.
pkgload::load_all()
tar_destroy()
tar_option_set(memory = "transient")
targets <- lapply(
  seq_len(50),
  function(index) {
    tar_target_raw(
      as.character(index),
      quote(runif(1e6)),
      memory = "transient"
    )
  }
)
pipeline <- tar_pipeline(targets)
local <- local_init(pipeline)
local$run()
pryr::object_size(pipeline)
pryr::object_size(local)

# Pipeline should still be acceptably small even with lots of targets.
pkgload::load_all()
tar_destroy()
pipeline <- tar_pipeline(
  tar_target(x, seq_len(1000)),
  tar_target(y, x, pattern = map(x))
)
local <- local_init(pipeline)
local$run()
pryr::object_size(pipeline)
x <- pipeline_get_target(pipeline, "x")
pryr::object_size(x)
tar_destroy()

# y's target object should be large when retrival is "main"
# and small when retrieval is "worker".
pkgload::load_all()
tar_option_set(retrieval = "main")
options(clustermq.scheduler = "multicore")
pipeline <- tar_pipeline(
  tar_target(
    x,
    data.frame(x = runif(5e6)),
    deployment = "main",
    format = "fst"
  ),
  tar_target(y, x, format = "fst")
)
cmq <- clustermq_init(pipeline)
debug(cmq$run_worker)
cmq$run()
# pryr::object_size(target) # Run in the debugger. # nolint
tar_destroy()
tar_option_set(retrieval = "worker")
pipeline <- tar_pipeline(
  tar_target(
    x,
    data.frame(x = runif(5e6)),
    deployment = "main",
    format = "fst"
  ),
  tar_target(y, x, format = "fst")
)
cmq <- clustermq_init(pipeline)
debug(cmq$run_worker)
cmq$run()
# pryr::object_size(target) # Run in the debugger. # nolint
tar_destroy()
