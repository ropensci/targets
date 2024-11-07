test_that("packages are actually loaded", {
  # Needs sge_clustermq.tmpl (in current directory).
  skip_on_cran()
  skip_on_os("windows")
  skip_if_not_installed("clustermq")
  tar_destroy()
  on.exit(tar_destroy(), add = TRUE)
  tar_script({
    options(
      clustermq.scheduler = "sge",
      clustermq.template = "sge_clustermq.tmpl"
    )
    tar_target(
      x,
      tibble(x = "x"),
      packages = "tibble"
    )
  })
  tar_make_clustermq()
  expect_equal(tar_read(x), tibble::tibble(x = "x"))
})

test_that("nontrivial common data with custom environment", {
  skip_on_cran()
  skip_if_not_installed("clustermq")
  tar_destroy()
  on.exit(tar_destroy())
  tar_script({
    options(
      clustermq.scheduler = "sge",
      clustermq.template = "sge_clustermq.tmpl"
    )
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

test_that("nontrivial globals with global environment", {
  skip_on_cran()
  skip_if_not_installed("clustermq")
  tar_destroy()
  on.exit(tar_destroy())
  tar_script({
    options(
      clustermq.scheduler = "sge",
      clustermq.template = "sge_clustermq.tmpl"
    )
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

test_that("branching plan on SGE", {
  # Needs sge_clustermq.tmpl (in current directory).
  skip_on_cran()
  skip_if_not_installed("clustermq")
  tar_runtime$fun <- "tar_make_clustermq"
  on.exit(tar_runtime$fun <- NULL)
  unlink("_targets", recursive = TRUE)
  on.exit(unlink("_targets", recursive = TRUE), add = TRUE)
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

test_that("Same with worker-side storage", {
  # Needs sge_clustermq.tmpl (in current directory).
  skip_on_cran()
  skip_if_not_installed("clustermq")
  tar_runtime$fun <- "tar_make_clustermq"
  on.exit(tar_runtime$fun <- NULL)
  unlink("_targets", recursive = TRUE)
  on.exit(unlink("_targets", recursive = TRUE), add = TRUE)
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
  skip_on_cran()
  skip_if_not_installed("clustermq")
  tar_runtime$fun <- "tar_make_clustermq"
  on.exit(tar_runtime$fun <- NULL)
  unlink("_targets", recursive = TRUE)
  on.exit(unlink(c("saved.out", "_targets"), recursive = TRUE), add = TRUE)
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
  out <- names(cmq$scheduler$progress$completed$envir)
  expect_equal(out, "x")
  saveRDS(2L, pipeline_get_target(pipeline, "x")$file$path)
  x <- tar_target_raw("x", quote(save1()), format = "file")
  pipeline <- pipeline_init(list(x))
  cmq <- clustermq_init(pipeline)
  cmq$run()
  out <- names(cmq$scheduler$progress$completed$envir)
  expect_equal(out, "x")
})

test_that("2 cores", {
  skip_on_cran()
  skip_if_not_installed("clustermq")
  tar_destroy()
  on.exit(tar_destroy())
  tar_script({
    options(
      clustermq.scheduler = "sge",
      clustermq.template = "sge_clustermq.tmpl"
    )
    resources <- tar_resources(
      clustermq = tar_resources_clustermq(
        template = list(cores = 2)
      )
    )
    tar_option_set(resources = resources)
    tar_target(x, {
      Sys.sleep(5)
      "y"
    })
  })
  tar_make_clustermq()
  expect_equal(tar_read(x), "y")
})

test_that("2 cores (unstructured resources)", {
  skip_on_cran()
  skip_if_not_installed("clustermq")
  tar_destroy()
  on.exit(tar_destroy())
  tar_script({
    options(
      clustermq.scheduler = "sge",
      clustermq.template = "sge_clustermq.tmpl"
    )
    resources <- tar_resources(
      clustermq = tar_resources_clustermq(template = list(cores = 2))
    )
    suppressWarnings(tar_option_set(resources = resources))
    suppressWarnings(
      tar_target(x, {
        Sys.sleep(5)
        "y"
      })
    )
  })
  suppressWarnings(tar_make_clustermq())
  expect_equal(tar_read(x), "y")
})
