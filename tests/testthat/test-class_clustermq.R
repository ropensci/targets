tar_test("clustermq$workers", {
  out <- clustermq_init(list(), workers = 3L)
  expect_equal(out$workers, 3L)
})

tar_test("workerless deployment works", {
  skip_on_os("windows")
  skip_if_not_installed("clustermq")
  x <- tar_target_raw("x", quote(1L), deployment = "main")
  y <- tar_target_raw("y", quote(x), deployment = "main")
  z <- tar_target_raw("z", quote(x + 1L), deployment = "main")
  pipeline <- pipeline_init(list(x, y, z))
  clustermq_init(pipeline)$run()
  expect_equal(target_read_value(x)$object, 1L)
  expect_equal(target_read_value(y)$object, 1L)
  expect_equal(target_read_value(z)$object, 2L)
  x <- tar_target_raw("x", quote(1L), deployment = "main")
  y <- tar_target_raw("y", quote(x), deployment = "main")
  z <- tar_target_raw("z", quote(x + 1L), deployment = "main")
  pipeline <- pipeline_init(list(x, y, z))
  out <- clustermq_init(pipeline)
  out$run()
  built <- names(out$scheduler$progress$built$envir)
  expect_equal(built, character(0))
})

tar_test("some targets up to date, some not", {
  skip_on_os("windows")
  skip_if_not_installed("clustermq")
  old <- getOption("clustermq.scheduler")
  options(clustermq.scheduler = "multicore")
  x <- tar_target_raw("x", quote(1L))
  y <- tar_target_raw("y", quote(x))
  pipeline <- pipeline_init(list(x, y))
  local <- local_init(pipeline)
  local$run()
  x <- tar_target_raw("x", quote(1L))
  y <- tar_target_raw("y", quote(x + 1L))
  pipeline <- pipeline_init(list(x, y))
  cmq <- clustermq_init(pipeline)
  cmq$run()
  out <- names(cmq$scheduler$progress$built$envir)
  expect_equal(out, "y")
  value <- target_read_value(pipeline_get_target(pipeline, "y"))
  expect_equal(value$object, 2L)
  options(clustermq.scheduler = old)
})

tar_test("clustermq algo can skip targets", {
  skip_on_cran()
  skip_on_os("windows")
  skip_if_not_installed("clustermq")
  old <- getOption("clustermq.scheduler")
  options(clustermq.scheduler = "multicore")
  x <- tar_target_raw("x", quote(1L))
  y <- tar_target_raw("y", quote(x))
  pipeline <- pipeline_init(list(x, y))
  local <- local_init(pipeline)
  local$run()
  unlink(file.path("_targets", "objects", "x"))
  x <- tar_target_raw("x", quote(1L))
  y <- tar_target_raw("y", quote(x))
  pipeline <- pipeline_init(list(x, y))
  cmq <- clustermq_init(pipeline)
  cmq$run()
  out <- names(cmq$scheduler$progress$built$envir)
  expect_equal(out, "x")
  expect_equal(tar_read(x), 1L)
  options(clustermq.scheduler = old)
})

tar_test("nontrivial common data", {
  skip_on_cran()
  skip_on_os("windows")
  skip_if_not_installed("clustermq")
  old <- getOption("clustermq.scheduler")
  options(clustermq.scheduler = "multicore")
  old_envir <- tar_option_get("envir")
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
  cmq <- clustermq_init(pipeline)
  cmq$run()
  value <- target_read_value(pipeline_get_target(pipeline, "x"))
  expect_equal(value$object, 3L)
  tar_option_set(envir = old_envir)
  options(clustermq.scheduler = old)
})

tar_test("clustermq with a dynamic file", {
  skip_on_cran()
  skip_on_os("windows")
  skip_if_not_installed("clustermq")
  old <- getOption("clustermq.scheduler")
  options(clustermq.scheduler = "multicore")
  old_envir <- tar_option_get("envir")
  envir <- new.env(parent = globalenv())
  tar_option_set(envir = envir)
  evalq({
    save1 <- function() {
      file <- "saved.out"
      saveRDS(1L, file)
      file
    }
  }, envir = envir)
  x <- tar_target_raw("x", quote(save1()), format = "file")
  pipeline <- pipeline_init(list(x))
  cmq <- clustermq_init(pipeline)
  cmq$run()
  out <- names(cmq$scheduler$progress$built$envir)
  expect_equal(out, "x")
  saveRDS(2L, pipeline_get_target(pipeline, "x")$store$file$path)
  x <- tar_target_raw("x", quote(save1()), format = "file")
  pipeline <- pipeline_init(list(x))
  cmq <- clustermq_init(pipeline)
  cmq$run()
  out <- names(cmq$scheduler$progress$built$envir)
  expect_equal(out, "x")
  options(clustermq.scheduler = old)
  tar_option_set(envir = old_envir)
})

tar_test("branching plan", {
  skip_on_cran()
  skip_on_os("windows")
  skip_if_not_installed("clustermq")
  old <- getOption("clustermq.scheduler")
  options(clustermq.scheduler = "multicore")
  pipeline <- pipeline_map()
  out <- clustermq_init(pipeline, workers = 2L)
  out$run()
  skipped <- names(out$scheduler$progress$skipped$envir)
  expect_equal(skipped, character(0))
  out2 <- clustermq_init(pipeline_map(), workers = 2L)
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
  options(clustermq.scheduler = old)
})

tar_test("clustermq$validate()", {
  out <- clustermq_init(pipeline_init())
  expect_silent(out$validate())
})
