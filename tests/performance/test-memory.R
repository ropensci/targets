# local_init(pipeline)$run() retains correct objects in persistent memory
library(testthat)
tar_destroy()
tar_option_set(envir = environment(), memory = "persistent")
pipeline <- pipeline_init(
  list(
    tar_target_raw(name = "data1", quote(seq_len(10))),
    tar_target_raw(name = "data2", quote(seq_len(20))),
    tar_target_raw(name = "min1", quote(min(data1))),
    tar_target_raw(name = "min2", quote(min(data2))),
    tar_target_raw(name = "max1", quote(max(data1))),
    tar_target_raw(name = "max2", quote(max(data2))),
    tar_target_raw(name = "mins", quote(c(min1, min2))),
    tar_target_raw(name = "maxes", quote(c(max1, max2))),
    tar_target_raw(
      name = "all",
      quote(c(browser(), mins, maxes))
    )
  )
)
# Enter the debugger:
local_init(pipeline)$run()
# Run these tests inside the debugger:
names <- pipeline_get_names(pipeline)
result <- map_lgl(names, ~is.null(pipeline_get_target(pipeline, .x)$value))
expect_true(result["all"])
expect_false(any(result[setdiff(names(result), "all")]))
# Exit the debugger.

# Same test with transient memory:
tar_destroy()
tar_option_set(envir = environment(), memory = "transient")
pipeline <- pipeline_init(
  list(
    tar_target_raw(name = "data1", quote(seq_len(10))),
    tar_target_raw(name = "data2", quote(seq_len(20))),
    tar_target_raw(name = "min1", quote(min(data1))),
    tar_target_raw(name = "min2", quote(min(data2))),
    tar_target_raw(name = "max1", quote(max(data1))),
    tar_target_raw(name = "max2", quote(max(data2))),
    tar_target_raw(name = "mins", quote(c(min1, min2))),
    tar_target_raw(name = "maxes", quote(c(max1, max2))),
    tar_target_raw(
      name = "all",
      quote(c(browser(), mins, maxes))
    )
  )
)
# Enter the debugger:
local_init(pipeline)$run()
# Run these tests inside the debugger:
names <- pipeline_get_names(pipeline)
result <- map_lgl(names, ~is.null(pipeline_get_target(pipeline, .x)$value))
expect_true(all(result[setdiff(names(result), c("mins", "maxes"))]))
expect_false(any(result[c("mins", "maxes")]))
# Exit the debugger.

# Clean up and restart.
tar_option_reset()
tar_destroy()
rstudioapi::restartSession()

# Memory usage tests:
library(pryr)
tar_script({
  # Comment out and see memory increase:
  tar_option_set(memory = "transient", garbage_collection = TRUE)
  lapply(
    paste0("x", seq_len(50)),
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
tar_destroy()
tar_option_set(memory = "transient")
targets <- lapply(
  paste0("x", seq_len(50)),
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
