tar_test("future workers actually launch (run interactively)", {
  skip_if_not_installed("future")
  skip_if_not_installed("future.callr")
  tar_script({
    future::plan(future.callr::callr)
    list(
      tar_target(x, seq_len(4)),
      tar_target(
        y,
        Sys.sleep(30),
        pattern = map(x)
      )
    )
  })
  # The following should run 4 targets concurrently.
  # Terminate early if necessary.
  tar_make_future(workers = 4)
  out <- tar_progress()
  out <- out[out$name != "x", ]
  expect_true(all(out$progress == "started"))
  tar_destroy()
})

# Run interactively:
tar_test("custom future plans through structured resources", {
  skip_if_not_installed("future")
  tar_script({
    future::plan(future::multisession, workers = 4)
    plan_multisession <- future::plan()
    future::plan(future::sequential)
    list(
      tar_target(x, seq_len(4)),
      tar_target(
        y,
        Sys.sleep(30),
        pattern = map(x),
        resources = tar_resources(
          future = tar_resources_future(plan = plan_multisession)
        )
      )
    )
  })
  # The following should run 4 targets concurrently.
  tar_make_future(workers = 4)
  # After all 4 targets start, terminate the pipeline early and show progress.
  # x should be built, and y and its 4 branches should be listed as started.
  out <- tar_progress()
  out <- out[out$name != "x", ]
  expect_true(all(out$progress == "started"))
  tar_destroy()
})

tar_test("custom future plans through unstructured resources", {
  skip_if_not_installed("future")
  tar_script({
    future::plan(future::multisession, workers = 4)
    plan_multisession <- future::plan()
    future::plan(future::sequential)
    suppressWarnings(
      list(
        tar_target(x, seq_len(4)),
        tar_target(
          y,
          Sys.sleep(30),
          pattern = map(x),
          resources = list(plan = plan_multisession)
        )
      )
    )
  })
  # The following should run 4 targets concurrently.
  tar_make_future(workers = 4)
  # After all 4 targets start, terminate the pipeline early and show progress.
  # x should be built, and y and its 4 branches should be listed as started.
  out <- tar_progress()
  out <- out[out$name != "x", ]
  expect_true(all(out$progress == "started"))
  tar_destroy()
})

tar_test("parallel workload should run fast", {
  skip_if_not_installed("future")
  skip_if_not_installed("future.callr")
  tar_script({
    library(targets)
    future::plan(future.callr::callr)
    list(
      tar_target(
        index_batch,
        seq_len(20),
      ),
      tar_target(
        data_continuous,
        index_batch,
        pattern = map(index_batch)
      ),
      tar_target(
        data_discrete,
        index_batch,
        pattern = map(index_batch)
      ),
      tar_target(
        fit_continuous,
        data_continuous,
        pattern = map(data_continuous)
      ),
      tar_target(
        fit_discrete,
        data_discrete,
        pattern = map(data_discrete)
      )
    )
  })
  tar_make_future(workers = 4, callr_function = NULL)
  expect_equal(tar_outdated(), character(0))
  expect_equal(unname(tar_read(fit_continuous)), seq_len(20))
  expect_equal(unname(tar_read(fit_discrete)), seq_len(20))
})

tar_test("profile parallel workload", {
  skip_if_not_installed("future")
  skip_if_not_installed("future.callr")
  tar_script({
    library(targets)
    future::plan(future.callr::callr)
    list(
      tar_target(
        index_batch,
        seq_len(10),
      ),
      tar_target(
        data_continuous,
        index_batch,
        pattern = map(index_batch)
      ),
      tar_target(
        data_discrete,
        index_batch,
        pattern = map(index_batch)
      ),
      tar_target(
        fit_continuous,
        data_continuous,
        pattern = map(data_continuous)
      ),
      tar_target(
        fit_discrete,
        data_discrete,
        pattern = map(data_discrete)
      )
    )
  })
  # Should deploy targets in a timely manner.
  proffer::pprof(tar_make_future(workers = 4, callr_function = NULL))
})

tar_test("prevent high-memory data via target objects", {
  skip_if_not_installed("future")
  skip_if_not_installed("future.callr")
  tar_runtime$set_fun("tar_make_future")
  on.exit(tar_runtime$unset_fun())
  # Run this test once inside tar_test() (test environment)
  # and once outside tar_test() global environment.
  future::plan(future.callr::callr)
  t <- list(tar_target(x, runif(1e7), deployment = "main", format = "qs"))
  pipeline <- pipeline_init(list(t[[1]], tar_target(y, x)))
  algo <- future_init(pipeline)
  debug(algo$update_globals)
  tar_option_set(envir = environment())
  # should enter a debugger:
  algo$run()
  # In the debugger verify that the exported data is much smaller than
  # the value of x because we cloned the target objects in pipeline_init().
  o <- self$produce_exports(tar_option_get("envir"), path_store_default())
  # Exported data should be small:
  pryr::object_size(o)
  # The target object should not be in the environment.
  expect_true(inherits(tar_option_get("envir")$t[[1]], "tar_target"))
  pryr::object_size(tar_option_get("envir")$t[[1]])
  # The pipeline's copy of the target object should be much larger:
  pryr::object_size(pipeline_get_target(self$pipeline, "x")$value$object)
  # The algorithm object itself should be large too, and it is not exported.
  pryr::object_size(self)
})
