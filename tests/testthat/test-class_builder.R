tar_test("builder$store", {
  x <- target_init(name = "abc", expr = quote(a), format = "rds")
  expect_silent(store_validate(x$store))
})

tar_test("builder$metrics", {
  x <- target_init(name = "abc", expr = quote(1L))
  expect_null(x$metrics)
  builder_update_build(x)
  expect_silent(metrics_validate(x$metrics))
})

tar_test("target_run() on a good builder", {
  x <- target_init(name = "abc", expr = quote(a))
  target_run(x, envir = tmpenv(a = "x"))
  expect_silent(metrics_validate(x$metrics))
  expect_silent(value_validate(x$value))
  expect_equal(x$value$object, "x")
  builder_update_object(x)
  expect_true(file.exists(x$store$file$path))
})

tar_test("target_run() on a errored builder", {
  local_init(pipeline_init())$start()
  x <- target_init(name = "abc", expr = quote(identity(identity(stop(123)))))
  target_run(x, tmpenv())
  meta <- meta_init()
  target_update_depend(x, pipeline_init(), meta)
  expect_error(
    target_conclude(x, pipeline_init(), scheduler_init(), meta),
    class = "tar_condition_run"
  )
  expect_null(x$value$object)
  expect_true(metrics_has_error(x$metrics))
})

tar_test("target_run_worker()", {
  local_init(pipeline_init())$start()
  x <- target_init(name = "abc", expr = quote(identity(identity(stop(123)))))
  y <- target_run_worker(x, tmpenv())
  expect_true(inherits(y, "tar_builder"))
  expect_silent(target_validate(y))
})

tar_test("builders with different names use different seeds", {
  a <- target_init(
    name = "a",
    expr = quote(sample.int(1e9, 1L))
  )
  b <- target_init(
    name = "b",
    expr = quote(sample.int(1e9, 1L))
  )
  c <- target_init(
    name = "b",
    expr = quote(sample.int(1e9, 1L))
  )
  builder_update_build(a, baseenv())
  builder_update_build(b, baseenv())
  builder_update_build(c, baseenv())
  expect_false(a$value$object == b$value$object)
  expect_equal(b$value$object, c$value$object)
})

tar_test("read and write objects", {
  x <- target_init(name = "abc", expr = quote(a), format = "rds")
  tmp <- tempfile()
  file <- x$store$file
  file$path <- tmp
  file$stage <- tempfile()
  builder_update_build(x, tmpenv(a = "123"))
  builder_update_object(x)
  expect_equal(readRDS(tmp), "123")
  expect_equal(target_read_value(x)$object, "123")
})

tar_test("error = \"stop\" means stop on error", {
  x <- target_init("x", expr = quote(stop(123)), error = "stop")
  y <- target_init("y", expr = quote(stop(456)), error = "stop")
  pipeline <- pipeline_init(list(x, y))
  expect_error(local_init(pipeline)$run(), class = "tar_condition_run")
  expect_equal(x$store$file$path, character(0))
  meta <- meta_init()$database$read_condensed_data()
  expect_true(all(nzchar(meta$error)))
  expect_equal(x$store$file$path, character(0))
  expect_equal(y$store$file$path, character(0))
})

tar_test("error = \"continue\" means continue on error", {
  x <- target_init("x", expr = quote(stop(123)), error = "continue")
  y <- target_init("y", expr = quote(stop(456)), error = "continue")
  pipeline <- pipeline_init(list(x, y))
  suppressWarnings(
    expect_warning(
      suppressMessages(local_init(pipeline)$run()),
      class = "tar_condition_run"
    )
  )
  expect_equal(x$store$file$path, character(0))
  expect_equal(y$store$file$path, character(0))
  meta <- meta_init()$database$read_condensed_data()
  expect_true(all(nzchar(meta$error)))
  expect_equal(x$store$file$path, character(0))
  expect_equal(y$store$file$path, character(0))
})

tar_test("errored targets are not up to date", {
  x <- target_init("x", expr = quote(123))
  pipeline <- pipeline_init(list(x))
  local_init(pipeline)$run()
  for (index in seq_len(2L)) {
    x <- target_init("x", expr = quote(stop(123)))
    pipeline <- pipeline_init(list(x))
    expect_error(
      local_init(pipeline)$run(),
      class = "tar_condition_run"
    )
  }
})

tar_test("same if we continue on error", {
  x <- target_init("x", expr = quote(123))
  pipeline <- pipeline_init(list(x))
  local_init(pipeline)$run()
  for (index in seq_len(2L)) {
    x <- target_init("x", expr = quote(stop(123)), error = "continue")
    pipeline <- pipeline_init(list(x))
    local <- local_init(pipeline)
    local$run()
    counter <- local$scheduler$progress$skipped
    out <- counter_get_names(counter)
    expect_equal(out, character(0))
  }
})

tar_test("builder writing from main", {
  local_init(pipeline_init())$start()
  x <- target_init("abc", expr = quote(a), format = "rds", storage = "main")
  pipeline <- pipeline_init(list(x))
  scheduler <- pipeline_produce_scheduler(pipeline)
  target_run(x, tmpenv(a = "123"))
  expect_false(file.exists(x$store$file$path))
  expect_true(is.na(x$store$file$hash))
  meta <- meta_init()
  memory_set_object(meta$depends, "abc", NA_character_)
  target_conclude(x, pipeline, scheduler, meta)
  expect_true(file.exists(x$store$file$path))
  expect_false(is.na(x$store$file$hash))
  path <- file.path("_targets", "objects", "abc")
  expect_equal(readRDS(path), "123")
  expect_equal(target_read_value(x)$object, "123")
  target_conclude(x, pipeline, scheduler, meta)
})

tar_test("builder writing from worker", {
  local_init(pipeline_init())$start()
  x <- target_init(
    "abc",
    expr = quote(a),
    format = "rds",
    storage = "worker",
    retrieval = "main",
    deployment = "worker"
  )
  target_run(x, tmpenv(a = "123"))
  expect_true(file.exists(x$store$file$path))
  expect_false(is.na(x$store$file$hash))
  path <- file.path("_targets", "objects", "abc")
  expect_equal(readRDS(path), "123")
  expect_equal(target_read_value(x)$object, "123")
  pipeline <- pipeline_init(list(x))
  scheduler <- pipeline_produce_scheduler(pipeline)
  meta <- meta_init()
  memory_set_object(meta$depends, "abc", NA_character_)
  target_conclude(x, pipeline, scheduler, meta)
})

tar_test("dynamic file writing from main", {
  local_init(pipeline_init())$start()
  envir <- new.env(parent = environment())
  x <- target_init(
    name = "abc",
    expr = quote(f()),
    format = "file",
    storage = "main"
  )
  envir$f <- function() {
    file <- tempfile()
    writeLines("lines", con = file)
    file
  }
  target_run(x, envir)
  expect_true(file.exists(x$store$file$path))
  expect_false(is.na(x$store$file$hash))
  pipeline <- pipeline_init(list(x))
  scheduler <- pipeline_produce_scheduler(pipeline)
  meta <- meta_init()
  memory_set_object(meta$depends, "abc", NA_character_)
  target_conclude(x, pipeline, scheduler, meta)
})

tar_test("dynamic file has illegal path", {
  x <- target_init(
    name = "abc",
    expr = quote("a*b"),
    format = "file"
  )
  local <- local_init(pipeline_init(list(x)))
  expect_error(local$run(), class = "tar_condition_validate")
})

tar_test("dynamic file has empty path", {
  x <- target_init(
    name = "abc",
    expr = quote(NULL),
    format = "file"
  )
  local <- local_init(pipeline_init(list(x)))
  expect_error(local$run(), class = "tar_condition_validate")
})

tar_test("dynamic file has missing path value", {
  x <- target_init(
    name = "abc",
    expr = quote(NA_character_),
    format = "file"
  )
  local <- local_init(pipeline_init(list(x)))
  expect_error(local$run(), class = "tar_condition_validate")
})

tar_test("dynamic file is missing at path", {
  x <- target_init(
    name = "abc",
    expr = quote("nope"),
    format = "file",
    deployment = "main"
  )
  local <- local_init(pipeline_init(list(x)))
  expect_error(local$run(), class = "tar_condition_validate")
})

tar_test("dynamic file writing from worker", {
  local_init(pipeline_init())$start()
  envir <- new.env(parent = environment())
  x <- target_init(
    name = "abc",
    expr = quote(f()),
    format = "file",
    storage = "worker",
    retrieval = "main"
  )
  envir$f <- function() {
    file <- tempfile()
    writeLines("lines", con = file)
    file
  }
  target_run(x, envir)
  expect_null(x$value)
  expect_true(file.exists(x$store$file$path))
  expect_false(is.na(x$store$file$hash))
  pipeline <- pipeline_init(list(x))
  scheduler <- pipeline_produce_scheduler(pipeline)
  meta <- meta_init()
  memory_set_object(meta$depends, "abc", NA_character_)
  target_conclude(x, pipeline, scheduler, meta)
})

tar_test("value kept if storage is local", {
  local_init(pipeline_init())$start()
  envir <- new.env(parent = environment())
  x <- target_init(
    name = "abc",
    expr = quote(f()),
    format = "file",
    storage = "main",
    retrieval = "main"
  )
  envir$f <- function() {
    file <- tempfile()
    writeLines("lines", con = file)
    file
  }
  target_run(x, envir)
  expect_equal(readLines(x$value$object), "lines")
})

tar_test("basic progress responses are correct", {
  local <- local_init(pipeline_order())
  pipeline_prune_names(local$pipeline, local$names)
  local$update_scheduler()
  progress <- local$scheduler$progress
  pipeline <- local$pipeline
  expect_equal(
    sort(counter_get_names(progress$queued)),
    sort(pipeline_get_names(pipeline))
  )
  expect_equal(sort(counter_get_names(progress$started)), character(0))
  expect_equal(sort(counter_get_names(progress$built)), character(0))
  expect_equal(sort(counter_get_names(progress$skipped)), character(0))
  expect_equal(sort(counter_get_names(progress$canceled)), character(0))
  expect_equal(sort(counter_get_names(progress$errored)), character(0))
  local$run()
  progress <- local$scheduler$progress
  expect_equal(sort(counter_get_names(progress$queued)), character(0))
  expect_equal(sort(counter_get_names(progress$started)), character(0))
  expect_equal(
    sort(counter_get_names(progress$built)),
    sort(pipeline_get_names(pipeline))
  )
  expect_equal(sort(counter_get_names(progress$skipped)), character(0))
  expect_equal(sort(counter_get_names(progress$canceled)), character(0))
  expect_equal(sort(counter_get_names(progress$errored)), character(0))
})

tar_test("builders load their packages", {
  x <- target_init(
    "x",
    quote(tibble(x = "x")),
    packages = "tibble"
  )
  pipeline <- pipeline_init(list(x))
  out <- local_init(pipeline)
  out$run()
  expect_equal(
    target_read_value(pipeline_get_target(pipeline, "x"))$object,
    tibble(x = "x")
  )
})

tar_test("relay errors as messages if error is continue", {
  tar_script({
    tar_option_set(error = "continue")
    list(
      tar_target(data1, stop("error_data1")),
      tar_target(data2, stop("error_data2"))
    )
  })
  suppressWarnings(
    expect_message(
      expect_warning(
        tar_make(callr_function = NULL),
        class = "tar_condition_run"
      ),
      class = "tar_condition_run"
    )
  )
  meta <- tar_meta(names = c("data1", "data2"), fields = error)
  expect_equal(sort(meta$error), sort(c("error_data1", "error_data2")))
})

tar_test("target_needs_worker(builder)", {
  x <- tar_target(y, rep(x, 2), deployment = "worker")
  expect_true(target_needs_worker(x))
  x <- tar_target(y, rep(x, 2), deployment = "main")
  expect_false(target_needs_worker(x))
})

tar_test("validate with nonmissing file and value", {
  x <- target_init(name = "abc", expr = quote(1L + 1L))
  x$value <- value_init(123)
  file <- x$store$file
  file$path <- tempfile()
  expect_silent(tmp <- target_validate(x))
})
