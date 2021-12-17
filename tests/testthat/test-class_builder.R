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
  tar_option_set(envir = tmpenv(a = "x"))
  x <- target_init(name = "abc", expr = quote(a))
  target_run(x, tar_option_get("envir"), path_store_default())
  expect_silent(metrics_validate(x$metrics))
  expect_silent(value_validate(x$value))
  expect_equal(x$value$object, "x")
  builder_update_object(x)
  expect_true(file.exists(x$store$file$path))
})

tar_test("target_run() on a errored builder", {
  tar_option_set(envir = tmpenv())
  local_init(pipeline_init())$start()
  x <- target_init(name = "abc", expr = quote(identity(identity(stop(123)))))
  target_run(x, tar_option_get("envir"), path_store_default())
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
  y <- target_run_worker(
    target = x,
    envir = tmpenv(),
    path_store = path_store_default(),
    fun = "tar_make",
    options = tar_options$export(),
    envvars = tar_envvars()
  )
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

tar_test("error = \"abridge\" means do not schedule new targets", {
  x <- target_init("x", expr = quote(stop(123)), error = "abridge")
  y <- target_init("y", expr = quote(x))
  pipeline <- pipeline_init(list(x, y))
  suppressWarnings(suppressMessages(local_init(pipeline)$run()))
  progress <- tar_progress()
  expect_equal(nrow(progress), 1L)
  expect_equal(progress$name, "x")
  expect_equal(progress$progress, "errored")
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
  tar_option_set(envir = tmpenv(a = "123"))
  local_init(pipeline_init())$start()
  x <- target_init("abc", expr = quote(a), format = "rds", storage = "main")
  pipeline <- pipeline_init(list(x))
  scheduler <- scheduler_init(pipeline, meta_init())
  target_run(x, tar_option_get("envir"), path_store_default())
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
  tar_option_set(envir = tmpenv(a = "123"))
  local_init(pipeline_init())$start()
  x <- target_init(
    "abc",
    expr = quote(a),
    format = "rds",
    storage = "worker",
    retrieval = "main",
    deployment = "worker"
  )
  target_run(x, tar_option_get("envir"), path_store_default())
  expect_true(file.exists(x$store$file$path))
  expect_false(is.na(x$store$file$hash))
  path <- file.path("_targets", "objects", "abc")
  expect_equal(readRDS(path), "123")
  expect_equal(target_read_value(x)$object, "123")
  pipeline <- pipeline_init(list(x))
  scheduler <- scheduler_init(pipeline, meta_init())
  meta <- meta_init()
  memory_set_object(meta$depends, "abc", NA_character_)
  target_conclude(x, pipeline, scheduler, meta)
})

tar_test("retrieval = \"none\"", {
  skip_on_cran()
  tar_script({
    list(
      tar_target(x, 1, memory = "transient"),
      tar_target(y, x, retrieval = "none")
    )
  })
  expect_error(
    tar_make(callr_function = NULL),
    class = "tar_condition_run"
  )
})

tar_test("storage = \"none\" errors if user does not write storage", {
  skip_on_cran()
  tar_script(tar_target(x, 1, storage = "none", memory = "persistent"))
  expect_error(tar_make(callr_function = NULL), class = "tar_condition_run")
})

tar_test("storage = \"none\" ignores return value but tracks file", {
  tar_script({
    run_x <- function() {
      if (!file.exists("_targets/objects")) {
        dir.create("_targets/objects")
      }
      saveRDS("correct", "_targets/objects/x")
      "incorrect"
    }
    list(
      tar_target(x, run_x(), storage = "none", memory = "persistent"),
      tar_target(y, x)
    )
  })
  tar_make(callr_function = NULL)
  expect_equal(tar_read(x), "correct")
  expect_equal(tar_read(y), "correct")
  hash <- tar_meta(x, data)$data
  tar_make(callr_function = NULL)
  expect_equal(tar_progress()$progress, rep("skipped", 2L))
  tar_script({
    run_x <- function() {
      if (!file.exists("_targets/objects")) {
        dir.create("_targets/objects")
      }
      saveRDS("correct", "_targets/objects/x")
      "incorrect"
      "incorrect"
    }
    list(
      tar_target(x, run_x(), storage = "none", memory = "persistent"),
      tar_target(y, x)
    )
  })
  tar_make(callr_function = NULL)
  expect_equal(tar_progress(x)$progress, "built")
  expect_equal(tar_progress(y)$progress, "skipped")
  expect_equal(hash, tar_meta(x, data)$data)
  tar_script({
    run_x <- function() {
      if (!file.exists("_targets/objects")) {
        dir.create("_targets/objects")
      }
      saveRDS("correct2", "_targets/objects/x")
      "incorrect"
      "incorrect"
    }
    list(
      tar_target(x, run_x(), storage = "none", memory = "persistent"),
      tar_target(y, x)
    )
  })
  tar_make(callr_function = NULL)
  expect_equal(tar_progress(x)$progress, "built")
  expect_equal(tar_progress(y)$progress, "built")
  expect_false(any(hash == tar_meta(x, data)$data))
})

tar_test("dynamic file writing from main", {
  local_init(pipeline_init())$start()
  envir <- new.env(parent = environment())
  tar_option_set(envir = envir)
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
  target_run(x, tar_option_get("envir"), path_store_default())
  expect_true(file.exists(x$store$file$path))
  expect_false(is.na(x$store$file$hash))
  pipeline <- pipeline_init(list(x))
  scheduler <- scheduler_init(pipeline, meta_init())
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
  expect_error(local$run(), class = "tar_condition_run")
})

tar_test("dynamic file has empty path", {
  x <- target_init(
    name = "abc",
    expr = quote(NULL),
    format = "file"
  )
  local <- local_init(pipeline_init(list(x)))
  expect_error(local$run(), class = "tar_condition_run")
})

tar_test("dynamic file has missing path value", {
  x <- target_init(
    name = "abc",
    expr = quote(NA_character_),
    format = "file"
  )
  local <- local_init(pipeline_init(list(x)))
  expect_error(local$run(), class = "tar_condition_run")
})

tar_test("dynamic file is missing at path", {
  x <- target_init(
    name = "abc",
    expr = quote("nope"),
    format = "file",
    deployment = "main"
  )
  local <- local_init(pipeline_init(list(x)))
  expect_error(local$run(), class = "tar_condition_run")
})

tar_test("dynamic file writing from worker", {
  local_init(pipeline_init())$start()
  envir <- new.env(parent = environment())
  tar_option_set(envir = envir)
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
  target_run(x, tar_option_get("envir"), path_store_default())
  expect_null(x$value)
  expect_true(file.exists(x$store$file$path))
  expect_false(is.na(x$store$file$hash))
  pipeline <- pipeline_init(list(x))
  meta <- meta_init()
  scheduler <- scheduler_init(pipeline, meta = meta)
  memory_set_object(meta$depends, "abc", NA_character_)
  target_conclude(x, pipeline, scheduler, meta)
})

tar_test("value kept if storage is local", {
  local_init(pipeline_init())$start()
  envir <- new.env(parent = environment())
  tar_option_set(envir = envir)
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
  target_run(x, tar_option_get("envir"), path_store_default())
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

tar_test("bootstrap builder for shortcut", {
  tar_script({
    list(
      tar_target(w, 1L),
      tar_target(x, w),
      tar_target(y, 1L),
      tar_target(z, x + y)
    )
  })
  tar_make(callr_function = NULL)
  expect_equal(tar_read(z), 2L)
  tar_script({
    list(
      tar_target(w, 1L),
      tar_target(x, w),
      tar_target(y, 1L),
      tar_target(z, x + y + 1L)
    )
  })
  tar_make(names = "z", shortcut = TRUE, callr_function = NULL)
  expect_equal(tar_read(z), 3L)
  progress <- tar_progress()
  expect_equal(nrow(progress), 1L)
  expect_equal(progress$name, "z")
  expect_equal(progress$progress, "built")
})

tar_test("informative error when bootstrap fails", {
  skip_on_cran()
  tar_script({
    list(
      tar_target(w, 1L),
      tar_target(x, w),
      tar_target(y, 1L),
      tar_target(z, x + y)
    )
  })
  expect_error(
    tar_make(names = "z", shortcut = TRUE, callr_function = NULL),
    class = "tar_condition_validate"
  )
})

tar_test("validate with nonmissing file and value", {
  x <- target_init(name = "abc", expr = quote(1L + 1L))
  x$value <- value_init(123)
  file <- x$store$file
  file$path <- tempfile()
  expect_silent(tmp <- target_validate(x))
})

tar_test("convert dep loading errors into runtime errors", {
  skip_on_cran()
  tar_script(
    list(
      tar_target(
        x1,
        stop("test"),
        priority = 1,
        error = "continue"
      ),
      tar_target(
        x2,
        x1,
        priority = 1,
        error = "continue"
      ),
      tar_target(
        x3,
        TRUE,
        priority = 0
      )
    )
  )
  suppressWarnings(tar_make(callr_function = NULL))
  expect_false(anyNA(tar_meta(x1)$error))
  expect_false(anyNA(tar_meta(x2)$error))
  expect_true(anyNA(tar_meta(x3)$error))
  expect_equal(tar_progress(x1)$progress, "errored")
  expect_equal(tar_progress(x2)$progress, "errored")
  expect_equal(tar_progress(x3)$progress, "built")
  expect_equal(tar_objects(), "x3")
  expect_true(tar_read(x3))
})
