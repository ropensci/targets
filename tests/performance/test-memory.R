library(targets)
library(pryr)

tar_script({
  # Comment out and see memory increase:
  tar_option_set(memory = "transient", garbage_collection = TRUE)
  lapply(
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
pipeline <- pipeline_init(targets)
local <- local_init(pipeline)
local$run()
pryr::object_size(pipeline)
pryr::object_size(local)

# Pipeline should still be acceptably small even with lots of targets.
pkgload::load_all()
tar_destroy()
pipeline <- pipeline_init(
  list(
    tar_target(x, seq_len(1000)),
    tar_target(y, x, pattern = map(x))
  )
)
local <- local_init(pipeline)
local$run()
pryr::object_size(pipeline)
x <- pipeline_get_target(pipeline, "x")
pryr::object_size(x)
tar_destroy()
