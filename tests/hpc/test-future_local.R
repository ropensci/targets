tar_test("future workers actually launch", {
  skip_on_cran()
  tar_script({
    future::plan(future::multisession)
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
  tar_progress()
})

tar_test("custom future plans through resources", {
  skip_on_cran()
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
        resources = list(plan = plan_multisession)
      )
    )
  })
  # The following should run 4 targets concurrently.
  tar_make_future(workers = 4)
  # After all 4 targets start, terminate the pipeline early and show progress.
  # x should be built, and y and its 4 branches should be listed as started.
  tar_progress()
})

tar_test("profile heavily parallel workload", {
  tar_script({
    library(targets)
    message("starting psock")
    future::plan(future::multisession, workers = 4)
    list(
      tar_target(
        index_batch,
        seq_len(100),
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
  expect_equal(tar_outdated(), character(0))
  expect_equal(tar_read(fit_continuous), seq_len(100))
  expect_equal(tar_read(fit_discrete), seq_len(100))
})
