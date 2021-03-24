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

tar_test("nontrivial globals with global environment", {
  skip_on_cran()
  skip_if_not_installed("clustermq")
  tar_script({
    options(clustermq.scheduler = "multiprocess")
    f <- function(x) {
      g(x) + 1L
    }
    g <- function(x) {
      x + 1L
    }
    list(
      tar_target(x, 1),
      tar_target(y, f(x))
    )
  })
  tar_make_clustermq()
  expect_equal(tar_read(y), 3L)
})

tar_test("prevent high-memory data via target objects in globalenv", {
  options(clustermq.scheduler = "multiprocess")
  t <- list(tar_target(x, runif(1e7), deployment = "main", format = "qs"))
  pipeline <- pipeline_init(list(t[[1]], tar_target(y, x)))
  algo <- clustermq_init(pipeline)
  debug(algo$set_common_data)
  # should enter a debugger:
  algo$run()
  # In the debugger verify that the exported data is much smaller than
  # the value of x because we cloned the target objects in pipeline_init().
  o <- self$produce_exports(envir)
  # Exported data should be small:
  pryr::object_size(o)
  # So should the target object in the global environment:
  expect_true(inherits(envir$t[[1]], "tar_target"))
  pryr::object_size(envir$t[[1]])
  # The pipeline's copy of the target object should be much larger:
  pryr::object_size(pipeline_get_target(self$pipeline, "x")$value$object)
  # as well as the algorithm object itself:
  pryr::object_size(self)
})

tar_test("profile heavily parallel workload", {
  tar_script({
    library(targets)
    options(clustermq.scheduler = "multicore")
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
