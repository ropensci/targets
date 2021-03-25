# y's target object should be large when retrival is "main"
# and small when retrieval is "worker".
tar_option_set(retrieval = "main")
options(clustermq.scheduler = "multicore")
pipeline <- pipeline_init(
  list(
    tar_target(
      x,
      data.frame(x = runif(5e6)),
      deployment = "main",
      format = "fst"
    ),
    tar_target(y, x, format = "fst")
  )
)
cmq <- clustermq_init(pipeline)
debug(cmq$run_worker)
cmq$run()
# The target object should be several MB.
# pryr::object_size(target) # Run in the debugger. # nolint
tar_destroy()
tar_option_set(retrieval = "worker")
pipeline <- pipeline_init(
  list(
    tar_target(
      x,
      data.frame(x = runif(5e6)),
      deployment = "main",
      format = "fst"
    ),
    tar_target(y, x, format = "fst")
  )
)
cmq <- clustermq_init(pipeline)
debug(cmq$run_worker)
cmq$run()
# The target object should be a lot smaller.
# pryr::object_size(target) # Run in the debugger. # nolint
tar_destroy()
