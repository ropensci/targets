# local_init(pipeline)$run() retains correct memory:
library(testthat)
tar_option_set()
pipeline <- pipeline_init(
  list(
    target_init(name = "data1", expr = quote(seq_len(10))),
    target_init(name = "data2", expr = quote(seq_len(20))),
    target_init(name = "min1", expr = quote(min(data1))),
    target_init(name = "min2", expr = quote(min(data2))),
    target_init(name = "max1", expr = quote(max(data1))),
    target_init(name = "max2", expr = quote(max(data2))),
    target_init(name = "mins", expr = quote(c(min1, min2))),
    target_init(name = "maxes", expr = quote(c(max1, max2))),
    target_init(
      name = "all",
      expr = quote(c(browser(), mins, maxes)),
      envir = globalenv()
    )
  )
)
# Enter the debugger:
local_init(pipeline)$run()
# Run these tests inside the debugger:
names <- paste0(rep(c("data", "min", "max"), each = 2), seq_len(2))
testthat::expect_equal(
  pipeline_get_target(pipeline, "mins")$cache$targets$names,
  character(0)
)
expect_equal(
  pipeline_get_target(pipeline, "mins")$cache$targets$names,
  character(0)
)
expect_equal(
  pipeline_get_target(pipeline, "mins")$cache$targets$names,
  character(0)
)
expect_false(is.null(pipeline_get_target(pipeline, "mins")$value))
expect_false(is.null(pipeline_get_target(pipeline, "maxes")$value))
expect_true(is.null(pipeline_get_target(pipeline, "all")$value))
expect_equal(
  sort(pipeline_get_target(pipeline, "all")$cache$targets$names),
  sort(c("maxes", "mins"))
)
# Exit the debugger and clean up.
tar_option_reset()
tar_destroy()
rstudioapi::restartSession()

# Memory usage tests:
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
