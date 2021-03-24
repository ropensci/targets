tar_test("clustermq iteration loop can wait and shut down workers", {
  skip_on_os("windows")
  skip_if_not_installed("clustermq")
  old <- getOption("clustermq.scheduler")
  options(clustermq.scheduler = "multiprocess")
  x <- tar_target_raw("x", quote(Sys.sleep(2)), garbage_collection = TRUE)
  y <- tar_target_raw("y", quote(list(x, a = "x")), garbage_collection = TRUE)
  pipeline <- pipeline_init(list(x, y))
  out <- clustermq_init(pipeline, reporter = "silent")
  out$run()
  target <- pipeline_get_target(pipeline, "y")
  expect_equal(target_read_value(target)$object$a, "x")
  options(clustermq.scheduler = old)
})

tar_test("profile heavily parallel workload", {
  tar_script({
    library(targets)
    options(clustermq.scheduler = "multiprocess")
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
  proffer::pprof(tar_make_clustermq(workers = 4, callr_function = NULL))
  expect_equal(tar_outdated(callr_function = NULL), character(0))
  expect_equal(tar_read(fit_continuous), seq_len(100))
  expect_equal(tar_read(fit_discrete), seq_len(100))
})
