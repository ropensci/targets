tar_test("clustermq$workers", {
  skip_cran()
  out <- clustermq_init(list(), workers = 3L)
  expect_equal(out$workers, 3L)
})

tar_test("workerless deployment works", {
  skip_cran()
  skip_on_os("windows")
  skip_on_os("solaris")
  require_clustermq()
  skip_on_covr()
  tar_runtime$fun <- "tar_make_clustermq"
  on.exit(tar_runtime$fun <- NULL)
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
  completed <- names(out$scheduler$progress$completed$envir)
  expect_equal(completed, character(0))
})

tar_test("semi-workerless deployment works", {
  skip_cran()
  skip_on_os("mac")
  skip_on_os("windows")
  skip_on_os("solaris")
  require_clustermq()
  skip_on_covr()
  tar_runtime$fun <- "tar_make_clustermq"
  on.exit(tar_runtime$fun <- NULL)
  old <- getOption("clustermq.scheduler")
  options(clustermq.scheduler = "multicore")
  on.exit(options(clustermq.scheduler = old), add = TRUE)
  x <- tar_target_raw("x", quote(1L), deployment = "main")
  y <- tar_target_raw("y", quote(x), deployment = "worker")
  z <- tar_target_raw("z", quote(x + 1L), deployment = "main")
  pipeline <- pipeline_init(list(x, y, z))
  # https://github.com/mschubert/clustermq/issues/269
  suppressWarnings(clustermq_init(pipeline)$run())
  expect_equal(target_read_value(x)$object, 1L)
  expect_equal(tar_read(y), 1L)
  expect_equal(target_read_value(z)$object, 2L)
  x <- tar_target_raw("x", quote(1L), deployment = "main")
  y <- tar_target_raw("y", quote(x), deployment = "worker")
  z <- tar_target_raw("z", quote(x + 1L), deployment = "main")
  pipeline <- pipeline_init(list(x, y, z))
  out <- clustermq_init(pipeline)
  out$run()
  completed <- names(out$scheduler$progress$completed$envir)
  expect_equal(completed, character(0))
})

tar_test("some targets up to date, some not", {
  skip_cran()
  skip_on_os("mac")
  skip_on_os("windows")
  skip_on_os("solaris")
  require_clustermq()
  skip_on_covr()
  old <- getOption("clustermq.scheduler")
  options(clustermq.scheduler = "multicore")
  tar_runtime$fun <- "tar_make_clustermq"
  on.exit(tar_runtime$fun <- NULL)
  on.exit(options(clustermq.scheduler = old), add = TRUE)
  x <- tar_target_raw("x", quote(1L))
  y <- tar_target_raw("y", quote(x))
  pipeline <- pipeline_init(list(x, y))
  local <- local_init(pipeline)
  local$run()
  x <- tar_target_raw("x", quote(1L))
  y <- tar_target_raw("y", quote(x + 1L))
  pipeline <- pipeline_init(list(x, y))
  # https://github.com/mschubert/clustermq/issues/269
  cmq <- clustermq_init(pipeline)
  suppressWarnings(cmq$run())
  out <- names(cmq$scheduler$progress$completed$envir)
  expect_equal(out, "y")
  value <- target_read_value(pipeline_get_target(pipeline, "y"))
  expect_equal(value$object, 2L)
})

tar_test("clustermq algo can skip targets", {
  skip_cran()
  skip_on_os("mac")
  skip_on_os("windows")
  skip_on_os("solaris")
  require_clustermq()
  skip_on_covr()
  old <- getOption("clustermq.scheduler")
  options(clustermq.scheduler = "multicore")
  tar_runtime$fun <- "tar_make_clustermq"
  on.exit(tar_runtime$fun <- NULL)
  on.exit(options(clustermq.scheduler = old), add = TRUE)
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
  # https://github.com/mschubert/clustermq/issues/269
  suppressWarnings(cmq$run())
  out <- names(cmq$scheduler$progress$completed$envir)
  expect_equal(out, "x")
  expect_equal(tar_read(x), 1L)
})

tar_test("nontrivial common data", {
  skip_cran()
  skip_on_os("mac")
  skip_on_os("windows")
  skip_on_os("solaris")
  skip_on_covr()
  require_clustermq()
  old <- getOption("clustermq.scheduler")
  options(clustermq.scheduler = "multicore")
  tar_runtime$fun <- "tar_make_clustermq"
  on.exit(tar_runtime$fun <- NULL)
  old_envir <- tar_option_get("envir")
  envir <- new.env(parent = globalenv())
  tar_option_set(envir = envir)
  on.exit({
    options(clustermq.scheduler = old)
    tar_option_set(envir = old_envir)
  }, add = TRUE)
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
  # https://github.com/mschubert/clustermq/issues/269
  suppressWarnings(cmq$run())
  value <- target_read_value(pipeline_get_target(pipeline, "x"))
  expect_equal(value$object, 3L)
})

tar_test("clustermq with a dynamic file", {
  skip_cran()
  skip_on_os("mac")
  skip_on_os("windows")
  skip_on_os("solaris")
  skip_on_covr()
  require_clustermq()
  old <- getOption("clustermq.scheduler")
  options(clustermq.scheduler = "multicore")
  tar_runtime$fun <- "tar_make_clustermq"
  on.exit(tar_runtime$fun <- NULL)
  old_envir <- tar_option_get("envir")
  envir <- new.env(parent = globalenv())
  on.exit({
    options(clustermq.scheduler = old)
    tar_option_set(envir = old_envir)
  }, add = TRUE)
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
  # https://github.com/mschubert/clustermq/issues/269
  suppressWarnings(cmq$run())
  out <- names(cmq$scheduler$progress$completed$envir)
  expect_equal(out, "x")
  saveRDS(2L, pipeline_get_target(pipeline, "x")$store$file$path)
  x <- tar_target_raw("x", quote(save1()), format = "file")
  pipeline <- pipeline_init(list(x))
  cmq <- clustermq_init(pipeline)
  # https://github.com/mschubert/clustermq/issues/269
  suppressWarnings(cmq$run())
  out <- names(cmq$scheduler$progress$completed$envir)
  expect_equal(out, "x")
})

tar_test("branching plan", {
  skip_cran()
  skip_on_os("mac")
  skip_on_os("windows")
  skip_on_os("solaris")
  require_clustermq()
  skip_on_covr()
  old <- getOption("clustermq.scheduler")
  options(clustermq.scheduler = "multicore")
  tar_runtime$fun <- "tar_make_clustermq"
  on.exit(tar_runtime$fun <- NULL)
  on.exit(options(clustermq.scheduler = old), add = TRUE)
  pipeline <- pipeline_map()
  out <- clustermq_init(pipeline, workers = 2L)
  # https://github.com/mschubert/clustermq/issues/269
  suppressWarnings(out$run())
  skipped <- names(out$scheduler$progress$skipped$envir)
  expect_equal(skipped, character(0))
  out2 <- clustermq_init(pipeline_map(), workers = 2L)
  # https://github.com/mschubert/clustermq/issues/269
  suppressWarnings(out2$run())
  completed <- names(out2$scheduler$progress$completed$envir)
  expect_equal(completed, character(0))
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

tar_test("cover the worker shutdown step in clustermq$iterate() event loop", {
  skip_cran()
  skip_on_os("mac")
  skip_on_os("windows")
  skip_on_os("solaris")
  require_clustermq()
  skip_on_covr()
  old <- getOption("clustermq.scheduler")
  options(clustermq.scheduler = "multicore")
  tar_runtime$fun <- "tar_make_clustermq"
  on.exit(tar_runtime$fun <- NULL)
  on.exit(options(clustermq.scheduler = old), add = TRUE)
  targets <- list(
    target_init("x1", quote(1)),
    target_init("x2", quote(x1)),
    target_init("x3", quote(x2)),
    target_init("x4", quote(x3))
  )
  pipeline <- pipeline_init(targets)
  out <- clustermq_init(pipeline, workers = 2L)
  # https://github.com/mschubert/clustermq/issues/269
  suppressWarnings(out$run())
  expect_equal(tar_read(x4), 1L)
})

tar_test("clustermq$validate()", {
  skip_cran()
  out <- clustermq_init(pipeline_init())
  expect_silent(out$validate())
})
