tar_test("future workers actually launch", {
  skip_on_cran()
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

test_that("packages are actually loaded", {
  # Needs sge_batchtools.tmpl (in current directory).
  unlink("_targets", recursive = TRUE)
  on.exit(unlink("_targets", recursive = TRUE))
  skip_if_not_installed("future")
  skip_if_not_installed("future.batchtools")
  on.exit(future::plan(future::sequential), add = TRUE)
  future::plan(
    future.batchtools::batchtools_sge,
    template = "sge_batchtools.tmpl"
  )
  old_envir <- tar_option_get("envir")
  on.exit(tar_option_set(envir = old_envir), add = TRUE)
  envir <- new.env(parent = globalenv())
  tar_option_set(envir = envir)
  x <- tar_target_raw(
    "x",
    quote(tibble(x = "x")),
    packages = "tibble"
  )
  pipeline <- pipeline_init(list(x))
  out <- future_init(pipeline)
  out$run()
  exp <- tibble::tibble(x = "x")
  target <- pipeline_get_target(pipeline, "x")
  expect_equal(tar_read(x), exp)
})

test_that("nontrivial globals", {
  unlink("_targets", recursive = TRUE)
  on.exit(unlink("_targets", recursive = TRUE))
  skip_if_not_installed("future")
  skip_if_not_installed("future.batchtools")
  on.exit(future::plan(future::sequential), add = TRUE)
  future::plan(
    future.batchtools::batchtools_sge,
    template = "sge_batchtools.tmpl"
  )
  old_envir <- tar_option_get("envir")
  on.exit(tar_option_set(envir = old_envir), add = TRUE)
  envir <- new.env(parent = globalenv())
  tar_option_set(envir = envir)
  evalq({
    f <- function(x) {
      g(x) + 1L
    }
    g <- function(x) {
      x + 1L
    }
  }, envir = envir)
  x <- tar_target_raw("x", quote(f(1L)))
  pipeline <- pipeline_init(list(x))
  algo <- future_init(pipeline)
  algo$run()
  target <- pipeline_get_target(pipeline, "x")
  expect_equal(tar_read(x), 3L)
})

test_that("branching plan on SGE", {
  # Needs sge_batchtools.tmpl (in current directory).
  unlink("_targets", recursive = TRUE)
  on.exit(unlink("_targets", recursive = TRUE))
  skip_if_not_installed("future")
  skip_if_not_installed("future.batchtools")
  on.exit(future::plan(future::sequential), add = TRUE)
  future::plan(
    future.batchtools::batchtools_sge,
    template = "sge_batchtools.tmpl"
  )
  pipeline <- pipeline_map()
  out <- future_init(pipeline, workers = 4L)
  out$run()
  skipped <- names(out$scheduler$progress$skipped$envir)
  expect_equal(skipped, character(0))
  out2 <- future_init(pipeline_map(), workers = 2L)
  out2$run()
  built <- names(out2$scheduler$progress$built$envir)
  expect_equal(built, character(0))
  value <- function(name) {
    target_read_value(pipeline_get_target(pipeline, name))$object
  }
  expect_equal(value("data0"), 2L)
  expect_equal(value("data1"), seq_len(3L))
  expect_equal(value("data2"), seq_len(3L) + 3L)
  branches <- target_get_children(pipeline_get_target(pipeline, "map1"))
  for (index in seq_along(branches)) {
    expect_equal(value(branches[index]), index + 2L)
  }
  branches <- target_get_children(pipeline_get_target(pipeline, "map2"))
  for (index in seq_along(branches)) {
    expect_equal(value(branches[index]), 2L * index + 3L)
  }
  branches <- target_get_children(pipeline_get_target(pipeline, "map3"))
  for (index in seq_along(branches)) {
    expect_equal(value(branches[index]), index + 3L)
  }
  branches <- target_get_children(pipeline_get_target(pipeline, "map4"))
  for (index in seq_along(branches)) {
    expect_equal(value(branches[index]), 3L * index + 5L)
  }
  branches <- target_get_children(pipeline_get_target(pipeline, "map5"))
  for (index in seq_along(branches)) {
    expect_equal(value(branches[index]), 2L * index + 5L)
  }
  branches <- target_get_children(pipeline_get_target(pipeline, "map6"))
  for (index in seq_along(branches)) {
    expect_equal(value(branches[index]), index + 15L)
  }
})

test_that("Same with worker-side storage", {
  # Needs sge_batchtools.tmpl (in current directory).
  unlink("_targets", recursive = TRUE)
  on.exit(unlink("_targets", recursive = TRUE))
  skip_if_not_installed("future")
  skip_if_not_installed("future.batchtools")
  on.exit(future::plan(future::sequential), add = TRUE)
  future::plan(
    future.batchtools::batchtools_sge,
    template = "sge_batchtools.tmpl"
  )
  pipeline <- pipeline_map(storage = "worker")
  out <- future_init(pipeline, workers = 4L)
  out$run()
  skipped <- names(out$scheduler$progress$skipped$envir)
  expect_equal(skipped, character(0))
  value <- function(name) {
    target_read_value(pipeline_get_target(pipeline, name))$object
  }
  expect_equal(value("data0"), 2L)
  expect_equal(value("data1"), seq_len(3L))
  expect_equal(value("data2"), seq_len(3L) + 3L)
  branches <- target_get_children(pipeline_get_target(pipeline, "map1"))
  for (index in seq_along(branches)) {
    expect_equal(value(branches[index]), index + 2L)
  }
  branches <- target_get_children(pipeline_get_target(pipeline, "map2"))
  for (index in seq_along(branches)) {
    expect_equal(value(branches[index]), 2L * index + 3L)
  }
  branches <- target_get_children(pipeline_get_target(pipeline, "map3"))
  for (index in seq_along(branches)) {
    expect_equal(value(branches[index]), index + 3L)
  }
  branches <- target_get_children(pipeline_get_target(pipeline, "map4"))
  for (index in seq_along(branches)) {
    expect_equal(value(branches[index]), 3L * index + 5L)
  }
  branches <- target_get_children(pipeline_get_target(pipeline, "map5"))
  for (index in seq_along(branches)) {
    expect_equal(value(branches[index]), 2L * index + 5L)
  }
  branches <- target_get_children(pipeline_get_target(pipeline, "map6"))
  for (index in seq_along(branches)) {
    expect_equal(value(branches[index]), index + 15L)
  }
})
