tar_test("clustermq iteration loop can wait and shut down workers", {
  skip_on_os("windows")
  skip_if_not_installed("clustermq")
  old <- getOption("clustermq.scheduler")
  options(clustermq.scheduler = "multicore")
  on.exit(options(clustermq.scheduler = old))
  x <- target_init("x", quote(Sys.sleep(2)))
  y <- target_init("y", quote(list(x, a = "x")))
  pipeline <- pipeline_init(list(x, y))
  out <- clustermq_init(pipeline, reporter = "silent")
  out$run()
  target <- pipeline_get_target(pipeline, "y")
  expect_equal(target_read_value(target)$object$a, "x")
})

test_that("packages are actually loaded on remote targets", {
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
  envir <- new.env(parent = globalenv())
  x <- target_init(
    "x",
    quote(tibble(x = "x")),
    packages = "tibble",
    envir = envir
  )
  pipeline <- pipeline_init(list(x))
  out <- clustermq_init(pipeline)
  out$run()
  exp <- tibble::tibble(x = "x")
  target <- pipeline_get_target(pipeline, "x")
  expect_equal(target_read_value(target)$object, exp)
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
  envir <- new.env(parent = baseenv())
  envir$f <- function(x) {
    g(x) + 1L
  }
  envir$g <- function(x) {
    x + 1L
  }
  x <- target_init("x", quote(f(1L)), envir = envir)
  pipeline <- pipeline_init(list(x))
  cmq <- clustermq_init(pipeline)
  cmq$run()
  target <- pipeline_get_target(pipeline, "x")
  expect_equal(target_read_value(target)$object, 3L)
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
  out <- clustermq_init(pipeline, garbage_collection = TRUE, workers = 4L)
  out$run()
  skipped <- counter_get_names(out$scheduler$progress$skipped)
  expect_equal(skipped, character(0))
  out2 <- clustermq_init(pipeline_map(), workers = 2L)
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

test_that("Same with remote storage", {
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
  pipeline <- pipeline_map(storage = "remote")
  out <- clustermq_init(pipeline, garbage_collection = TRUE, workers = 4L)
  out$run()
  skipped <- counter_get_names(out$scheduler$progress$skipped)
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
  envir <- new.env(parent = baseenv())
  envir$save1 <- function() {
    file <- "saved.out"
    saveRDS(1L, file)
    file
  }
  x <- target_init("x", quote(save1()), format = "file", envir = envir)
  pipeline <- pipeline_init(list(x))
  cmq <- clustermq_init(pipeline)
  cmq$run()
  out <- counter_get_names(cmq$scheduler$progress$built)
  expect_equal(out, "x")
  saveRDS(2L, pipeline_get_target(pipeline, "x")$store$file$path)
  x <- target_init("x", quote(save1()), format = "file", envir = envir)
  pipeline <- pipeline_init(list(x))
  cmq <- clustermq_init(pipeline)
  cmq$run()
  out <- counter_get_names(cmq$scheduler$progress$built)
  expect_equal(out, "x")
})
