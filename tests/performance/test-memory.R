library(targets)
library(pryr)

tar_script({
 tar_options(memory = "transient") # Comment out and see memory increase.
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
tar_make(garbage_collection = TRUE)

# The final size of the pipeline object should be small.
devtools::load_all()
tar_destroy()
tar_options(memory = "transient")
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
local <- algorithm_init("local", pipeline)
local$run()
pryr::object_size(pipeline)
pryr::object_size(local)

# Pipeline should still be acceptably small even with lots of targets.
devtools::load_all()
tar_destroy()
pipeline <- tar_pipeline(
  tar_target(x, seq_len(1000)),
  tar_target(y, x, pattern = map(x))
)
local <- algorithm_init("local", pipeline)
local$run()
pryr::object_size(pipeline)
x <- pipeline_get_target(pipeline, "x")
pryr::object_size(x)
