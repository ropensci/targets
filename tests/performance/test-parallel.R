# This interactive test checks parallel efficiency.
# We should not have to wait for all the `x2` branches to finish
# before we move on to `x3`-`x6` branches downstream of the
# first x1 branches.
library(targets)
tar_script({
  options(clustermq.scheduler = "multicore")
  list(
    tar_target(x1, c(0, 0, 5)),
    tar_target(x2, Sys.sleep(x1), pattern = map(x1)),
    tar_target(x3, x2, pattern = map(x2)),
    tar_target(x4, x3, pattern = map(x3)),
    tar_target(x5, x4, pattern = map(x4)),
    tar_target(x6, x5, pattern = map(x5))
  )
})
tar_make_clustermq(workers = 3L, reporter = "timestamp")
tar_destroy()

# If there are problems, inspect the example below.
envir <- globalenv()
pipeline <- pipeline_init(
  list(
    target_init("x1", quote(c(0, 0, 5))),
    target_init(
      "x2",
      quote(browser()),
      pattern = quote(map(x1)),
      deps = "x1",
      envir = envir
    ),
    target_init(
      "x3",
      quote(browser()),
      pattern = quote(map(x2)),
      deps = "x2",
      envir = envir
    ),
    target_init(
      "x4",
      quote(browser()),
      pattern = quote(map(x3)),
      deps = "x3",
      envir = envir
    ),
    target_init(
      "x5",
      quote(browser()),
      pattern = quote(map(x4)),
      deps = "x4",
      envir = envir
    )
  )
)
local <- local_init(pipeline = pipeline)
scheduler <- local$scheduler
local$run()
# scheduler$queue$data # nolint
# should show downstream branches ready to build.
# tar_name() # nolint
# should show the target currently running.
tar_destroy()
