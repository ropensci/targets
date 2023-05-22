tar_test("packages are actually loaded", {
  skip_on_cran()
  skip_if_not_installed("crew")
  tar_runtime$fun <- "tar_make"
  on.exit(tar_runtime$fun <- NULL)
  tar_option_set(envir = environment())
  x <- tar_target_raw(
    "x",
    quote(tibble(x = "x")),
    packages = "tibble"
  )
  pipeline <- pipeline_init(list(x))
  controller <- crew::crew_controller_local()
  out <- crew_init(pipeline, controller = controller)
  out$run()
  exp <- tibble::tibble(x = "x")
  expect_equal(tar_read(x), exp)
})

tar_test("crew iteration loop can wait for and shut down workers", {
  skip_on_os("windows")
  skip_if_not_installed("crew")
  tar_runtime$fun <- "tar_make"
  on.exit(tar_runtime$fun <- NULL)
  x <- tar_target_raw("x", quote(Sys.sleep(2)), garbage_collection = TRUE)
  y <- tar_target_raw("y", quote(list(x, a = "x")), garbage_collection = TRUE)
  pipeline <- pipeline_init(list(x, y))
  controller <- crew::crew_controller_local()
  out <- crew_init(pipeline, controller = controller, reporter = "silent")
  out$run()
  target <- pipeline_get_target(pipeline, "y")
  expect_equal(target_read_value(target)$object$a, "x")
})

tar_test("nontrivial globals with global environment", {
  skip_on_cran()
  skip_if_not_installed("crew")
  tar_script({
    controller <- crew::crew_controller_local()
    tar_option_set(controller = controller)
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
  tar_make()
  expect_equal(tar_read(y), 3L)
})

tar_test("prevent high-memory data via target objects", {
  # Run this test once inside tar_test() (test environment)
  # and once outside tar_test() global environment.
  skip_on_cran()
  skip_if_not_installed("crew")
  tar_runtime$fun <- "tar_make"
  on.exit(tar_runtime$fun <- NULL)
  t <- list(tar_target(x, runif(1e7), deployment = "main", format = "qs"))
  pipeline <- pipeline_init(list(t[[1]], tar_target(y, x)))
  controller <- crew::crew_controller_local()
  algo <- crew_init(pipeline, controller = controller)
  debug(algo$ensure_exports)
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
  skip_if_not_installed("crew")
  tar_script({
    library(targets)
    controller <- crew::crew_controller_local(workers = 4)
    tar_option_set(controller = controller)
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
  tar_make()
  expect_equal(tar_outdated(callr_function = NULL), character(0))
})

tar_test("saturated controllers should not get tasks", {
  # Also watch CPU usage on htop. Should be low.
  skip_on_cran()
  skip_if_not_installed("crew")
  tar_script({
    library(targets)
    controller <- crew::crew_controller_local(workers = 2)
    tar_option_set(controller = controller)
    list(
      tar_target(w, Sys.sleep(10)),
      tar_target(x, Sys.sleep(10)),
      tar_target(y, Sys.sleep(10)),
      tar_target(z, Sys.sleep(10))
    )
  })
  tar_make() # First two should start, then a pause, then the next two.
  expect_equal(tar_outdated(callr_function = NULL), character(0))
})

tar_test("controllers are assessed individually for saturation in a group", {
  skip_on_cran()
  skip_if_not_installed("crew")
  tar_script({
    library(targets)
    a <- crew::crew_controller_local(name = "a", workers = 2)
    b <- crew::crew_controller_local(name = "b", workers = 2)
    tar_option_set(controller = crew::crew_controller_group(a, b))
    resources_a <- tar_resources(crew = tar_resources_crew(controller = "a"))
    resources_b <- tar_resources(crew = tar_resources_crew(controller = "b"))
    list(
      tar_target(w, Sys.sleep(5), resources = resources_a),
      tar_target(x, Sys.sleep(5), resources = resources_a),
      tar_target(y, Sys.sleep(5), resources = resources_b),
      tar_target(z, Sys.sleep(5), resources = resources_b)
    )
  })
  tar_make() # All 4 should start at once and finish at once.
  expect_equal(tar_outdated(callr_function = NULL), character(0))
})
