tar_test("clustermq iteration loop can wait and shut down workers", {
  skip_on_os("windows")
  skip_if_not_installed("clustermq")
  old <- getOption("clustermq.scheduler")
  options(clustermq.scheduler = "multicore")
  x <- tar_target_raw("x", quote(Sys.sleep(2)), garbage_collection = TRUE)
  y <- tar_target_raw("y", quote(list(x, a = "x")), garbage_collection = TRUE)
  pipeline <- pipeline_init(list(x, y))
  out <- clustermq_init(pipeline, reporter = "silent")
  out$run()
  target <- pipeline_get_target(pipeline, "y")
  expect_equal(target_read_value(target)$object$a, "x")
  options(clustermq.scheduler = old)
})

test_that("packages are actually loaded", {
  # Needs sge_clustermq.tmpl (in current directory).
  unlink("_targets", recursive = TRUE)
  on.exit(unlink("_targets", recursive = TRUE))
  skip_on_os("windows")
  skip_if_not_installed("clustermq")
  old_schd <- getOption("clustermq.scheduler")
  old_tmpl <- getOption("clustermq.template")
  options(
    clustermq.scheduler = "sge",
    clustermq.template = "sge_clustermq.tmpl"
  )
  on.exit(
    options(
      clustermq.scheduler = old_schd,
      clustermq.template = old_tmpl
    ),
    add = TRUE
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
  out <- clustermq_init(pipeline)
  out$run()
  exp <- tibble::tibble(x = "x")
  expect_equal(tar_read(x), exp)
})

test_that("nontrivial common data", {
  unlink("_targets", recursive = TRUE)
  on.exit(unlink("_targets", recursive = TRUE))
  skip_on_os("windows")
  skip_if_not_installed("clustermq")
  old_schd <- getOption("clustermq.scheduler")
  old_tmpl <- getOption("clustermq.template")
  options(
    clustermq.scheduler = "sge",
    clustermq.template = "sge_clustermq.tmpl"
  )
  on.exit(
    options(
      clustermq.scheduler = old_schd,
      clustermq.template = old_tmpl
    ),
    add = TRUE
  )
  old_envir <- tar_option_get("envir")
  on.exit(tar_option_set(envir = old_envir), add = TRUE)
  envir <- new.env(parent = globalenv())
  tar_option_set(envir = envir)
  envir$f <- function(x) {
    g(x) + 1L
  }
  envir$g <- function(x) {
    x + 1L
  }
  x <- tar_target_raw("x", quote(f(1L)))
  pipeline <- pipeline_init(list(x))
  cmq <- clustermq_init(pipeline)
  cmq$run()
  target <- pipeline_get_target(pipeline, "x")
  expect_equal(tar_read(x), 3L)
})

test_that("branching plan on SGE", {
  # Needs sge_clustermq.tmpl (in current directory).
  unlink("_targets", recursive = TRUE)
  on.exit(on.exit(unlink("_targets", recursive = TRUE)))
  skip_on_os("windows")
  skip_if_not_installed("clustermq")
  old_schd <- getOption("clustermq.scheduler")
  old_tmpl <- getOption("clustermq.template")
  options(
    clustermq.scheduler = "sge",
    clustermq.template = "sge_clustermq.tmpl"
  )
  on.exit(
    options(
      clustermq.scheduler = old_schd,
      clustermq.template = old_tmpl
    ),
    add = TRUE
  )
  pipeline <- pipeline_map()
  out <- clustermq_init(pipeline, workers = 4L)
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
})

test_that("Same with worker-side storage", {
  # Needs sge_clustermq.tmpl (in current directory).
  unlink("_targets", recursive = TRUE)
  on.exit(unlink("_targets", recursive = TRUE))
  skip_on_os("windows")
  skip_if_not_installed("clustermq")
  old_schd <- getOption("clustermq.scheduler")
  old_tmpl <- getOption("clustermq.template")
  options(
    clustermq.scheduler = "sge",
    clustermq.template = "sge_clustermq.tmpl"
  )
  on.exit(
    options(
      clustermq.scheduler = old_schd,
      clustermq.template = old_tmpl
    ),
    add = TRUE
  )
  pipeline <- pipeline_map(storage = "worker")
  out <- clustermq_init(pipeline, workers = 4L)
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

test_that("clustermq with a dynamic file", {
  # Needs sge_clustermq.tmpl (in current directory).
  unlink("_targets", recursive = TRUE)
  on.exit(unlink(c("saved.out", "_targets"), recursive = TRUE))
  skip_on_os("windows")
  skip_if_not_installed("clustermq")
  old_schd <- getOption("clustermq.scheduler")
  old_tmpl <- getOption("clustermq.template")
  options(
    clustermq.scheduler = "sge",
    clustermq.template = "sge_clustermq.tmpl"
  )
  on.exit(
    options(
      clustermq.scheduler = old_schd,
      clustermq.template = old_tmpl
    ),
    add = TRUE
  )
  old_envir <- tar_option_get("envir")
  on.exit(tar_option_set(envir = old_envir), add = TRUE)
  envir <- new.env(parent = globalenv())
  tar_option_set(envir = envir)
  envir$save1 <- function() {
    file <- "saved.out"
    saveRDS(1L, file)
    file
  }
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
})
