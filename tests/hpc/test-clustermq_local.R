tar_test("packages are actually loaded", {
  skip_on_cran()
  skip_if_not_installed("clustermq")
  tar_runtime$set_fun("tar_make_clustermq")
  on.exit(tar_runtime$unset_fun())
  options(clustermq.scheduler = "multicore")
  tar_option_set(envir = environment())
  x <- tar_target_raw(
    "x",
    quote(tibble(x = "x")),
    packages = "tibble"
  )
  pipeline <- pipeline_init(list(x))
  out <- clustermq_init(pipeline)
  out$run()
  exp <- tibble::tibble(x = "x")
  expect_equal(tar_read(x), exp)
})

tar_test("clustermq iteration loop can wait for and shut down workers", {
  skip_on_os("windows")
  skip_if_not_installed("clustermq")
  tar_runtime$set_fun("tar_make_clustermq")
  on.exit(tar_runtime$unset_fun())
  on.exit(options(clustermq.scheduler = old), add = TRUE)
  old <- getOption("clustermq.scheduler")
  options(clustermq.scheduler = "multicore")
  x <- tar_target_raw("x", quote(Sys.sleep(2)), garbage_collection = TRUE)
  y <- tar_target_raw("y", quote(list(x, a = "x")), garbage_collection = TRUE)
  pipeline <- pipeline_init(list(x, y))
  out <- clustermq_init(pipeline, reporter = "silent")
  out$run()
  target <- pipeline_get_target(pipeline, "y")
  expect_equal(target_read_value(target)$object$a, "x")
})

tar_test("nontrivial common data with multicore", {
  skip_on_cran()
  skip_if_not_installed("clustermq")
  tar_script({
    options(clustermq.scheduler = "multicore")
    envir <- new.env(parent = globalenv())
    evalq({
      f <- function(x) {
        g(x) + 1L
      }
      g <- function(x) {
        x + 1L
      }
    }, envir = envir)
    tar_option_set(envir = envir)
    list(
      tar_target(x, 1),
      tar_target(y, f(x))
    )
  })
  tar_make_clustermq()
  expect_equal(tar_read(y), 3L)
})

tar_test("nontrivial globals with global environment", {
  skip_on_cran()
  skip_if_not_installed("clustermq")
  tar_script({
    options(clustermq.scheduler = "multicore")
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

tar_test("prevent high-memory data via target objects", {
  # Run this test once inside tar_test() (test environment)
  # and once outside tar_test() global environment.
  skip_on_cran()
  skip_if_not_installed("clustermq")
  tar_runtime$set_fun("tar_make_clustermq")
  on.exit(tar_runtime$unset_fun())
  options(clustermq.scheduler = "multicore")
  t <- list(tar_target(x, runif(1e7), deployment = "main", format = "qs"))
  pipeline <- pipeline_init(list(t[[1]], tar_target(y, x)))
  algo <- clustermq_init(pipeline)
  debug(algo$set_common_data)
  tar_option_set(envir = environment())
  # should enter a debugger:
  algo$run()
  # In the debugger verify that the exported data is much smaller than
  # the value of x because we cloned the target objects in pipeline_init().
  o <- self$produce_exports(envir, path_store_default())
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

tar_test("heavily parallel workload should run fast", {
  skip_on_cran()
  skip_if_not_installed("clustermq")
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
  tar_make_clustermq(workers = 4)
  expect_equal(tar_outdated(callr_function = NULL), character(0))
})

tar_test("profile heavily parallel workload", {
  skip_on_cran()
  skip_if_not_installed("clustermq")
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
  expect_equal(unname(tar_read(fit_continuous)), seq_len(100))
  expect_equal(unname(tar_read(fit_discrete)), seq_len(100))
})
