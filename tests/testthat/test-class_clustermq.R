tar_test("clustermq$workers", {
  out <- algorithm_init("clustermq", pipeline_init(), workers = 3L)
  expect_equal(out$workers, 3L)
})

tar_test("clustermq$template", {
  out <- algorithm_init(
    "clustermq",
    pipeline_init(),
    template = list(cores = 3L)
  )
  expect_equal(out$template, list(cores = 3L))
})

tar_test("all local deployment works", {
  skip_on_os("windows")
  skip_if_not_installed("clustermq")
  x <- target_init("x", quote(1L), deployment = "local")
  y <- target_init("y", quote(x), deployment = "local")
  z <- target_init("z", quote(x + 1L), deployment = "local")
  pipeline <- pipeline_init(list(x, y, z))
  algorithm_init("clustermq", pipeline)$run()
  expect_equal(target_read_value(x)$object, 1L)
  expect_equal(target_read_value(y)$object, 1L)
  expect_equal(target_read_value(z)$object, 2L)
  x <- target_init("x", quote(1L), deployment = "local")
  y <- target_init("y", quote(x), deployment = "local")
  z <- target_init("z", quote(x + 1L), deployment = "local")
  pipeline <- pipeline_init(list(x, y, z))
  out <- algorithm_init("clustermq", pipeline)
  built <- counter_get_names(out$scheduler$progress$built)
  expect_equal(built, character(0))
})

tar_test("some targets up to date, some not", {
  skip_on_os("windows")
  skip_if_not_installed("clustermq")
  old <- getOption("clustermq.scheduler")
  options(clustermq.scheduler = "multicore")
  on.exit(options(clustermq.scheduler = old))
  x <- target_init("x", quote(1L))
  y <- target_init("y", quote(x))
  pipeline <- pipeline_init(list(x, y))
  local <- local_init(pipeline)
  local$run()
  x <- target_init("x", quote(1L))
  y <- target_init("y", quote(x + 1L))
  pipeline <- pipeline_init(list(x, y))
  cmq <- algorithm_init("clustermq", pipeline)
  cmq$run()
  out <- counter_get_names(cmq$scheduler$progress$built)
  expect_equal(out, "y")
  value <- target_read_value(pipeline_get_target(pipeline, "y"))
  expect_equal(value$object, 2L)
})

tar_test("clustermq algo can skip targets", {
  skip_on_cran()
  skip_on_os("windows")
  skip_if_not_installed("clustermq")
  old <- getOption("clustermq.scheduler")
  options(clustermq.scheduler = "multicore")
  on.exit(options(clustermq.scheduler = old))
  x <- target_init("x", quote(1L))
  y <- target_init("y", quote(x))
  pipeline <- pipeline_init(list(x, y))
  local <- local_init(pipeline)
  local$run()
  unlink(file.path("_targets", "objects", "x"))
  x <- target_init("x", quote(1L))
  y <- target_init("y", quote(x))
  pipeline <- pipeline_init(list(x, y))
  cmq <- algorithm_init("clustermq", pipeline)
  cmq$run()
  out <- counter_get_names(cmq$scheduler$progress$built)
  expect_equal(out, "x")
  value <- target_read_value(pipeline_get_target(pipeline, "x"))
  expect_equal(value$object, 1L)
})

tar_test("nontrivial common data", {
  skip_on_cran()
  skip_on_os("windows")
  skip_if_not_installed("clustermq")
  old <- getOption("clustermq.scheduler")
  options(clustermq.scheduler = "multicore")
  on.exit(options(clustermq.scheduler = old))
  envir <- new.env(parent = baseenv())
  evalq({
    f <- function(x) {
      g(x) + 1L
    }
    g <- function(x) {
      x + 1L
    }
  }, envir = envir)
  x <- target_init("x", quote(f(1L)), envir = envir)
  pipeline <- pipeline_init(list(x))
  cmq <- algorithm_init("clustermq", pipeline)
  cmq$run()
  value <- target_read_value(pipeline_get_target(pipeline, "x"))
  expect_equal(value$object, 3L)
})

tar_test("clustermq with a dynamic file", {
  skip_on_cran()
  skip_on_os("windows")
  skip_if_not_installed("clustermq")
  old <- getOption("clustermq.scheduler")
  options(clustermq.scheduler = "multicore")
  on.exit(options(clustermq.scheduler = old))
  envir <- new.env(parent = baseenv())
  evalq({
    save1 <- function() {
      file <- "saved.out"
      saveRDS(1L, file)
      file
    }
  }, envir = envir)
  x <- target_init("x", quote(save1()), format = "file", envir = envir)
  pipeline <- pipeline_init(list(x))
  cmq <- algorithm_init("clustermq", pipeline)
  cmq$run()
  out <- counter_get_names(cmq$scheduler$progress$built)
  expect_equal(out, "x")
  saveRDS(2L, pipeline_get_target(pipeline, "x")$store$file$path)
  x <- target_init("x", quote(save1()), format = "file", envir = envir)
  pipeline <- pipeline_init(list(x))
  cmq <- algorithm_init("clustermq", pipeline)
  cmq$run()
  out <- counter_get_names(cmq$scheduler$progress$built)
  expect_equal(out, "x")
})

tar_test("branching plan", {
  skip_on_cran()
  skip_on_os("windows")
  skip_if_not_installed("clustermq")
  old <- getOption("clustermq.scheduler")
  options(clustermq.scheduler = "multicore")
  on.exit(options(clustermq.scheduler = old))
  pipeline <- pipeline_map()
  out <- algorithm_init(
    "clustermq",
    pipeline,
    workers = 2L,
    garbage_collection = TRUE
  )
  out$run()
  skipped <- counter_get_names(out$scheduler$progress$skipped)
  expect_equal(skipped, character(0))
  out2 <- algorithm_init("clustermq", pipeline_map(), workers = 2L)
  out2$run()
  built <- counter_get_names(out2$scheduler$progress$built)
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

tar_test("clustermq$validate()", {
  out <- algorithm_init("clustermq", pipeline_init())
  expect_silent(out$validate())
})
