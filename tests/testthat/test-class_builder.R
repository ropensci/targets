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
  dir_create(dirname(x$file$stage))
  dir_create(dirname(x$file$path))
  builder_update_object(x)
  expect_true(file.exists(x$file$path))
})

tar_test("target_run() on a errored builder", {
  skip_cran()
  tar_option_set(envir = tmpenv())
  local_init(pipeline_init())$start()
  x <- target_init(name = "abc", expr = quote(identity(identity(stop(123)))))
  target_run(x, tar_option_get("envir"), path_store_default())
  meta <- meta_init()
  on.exit(meta$database$close())
  target_update_depend(x, pipeline_init(), meta)
  expect_error(
    target_conclude(x, pipeline_init(), scheduler_init(), meta),
    class = "tar_condition_run"
  )
  expect_null(x$value$object)
  expect_true(metrics_has_error(x$metrics))
})

tar_test("target_run_worker()", {
  skip_cran()
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
  file <- x$file
  file$path <- tmp
  file$stage <- tempfile()
  builder_update_build(x, tmpenv(a = "123"))
  builder_update_object(x)
  expect_equal(readRDS(tmp), "123")
  expect_equal(target_read_value(x)$object, "123")
})

tar_test("error = \"stop\" means stop on error", {
  skip_cran()
  x <- target_init("x", expr = quote(stop(123)), error = "stop")
  y <- target_init("y", expr = quote(stop(456)), error = "stop")
  pipeline <- pipeline_init(list(x, y))
  expect_error(local_init(pipeline)$run(), class = "tar_condition_run")
  expect_equal(x$file$path, character(0))
  meta <- meta_init()$database$read_condensed_data()
  expect_true(all(nzchar(meta$error)))
  expect_equal(x$file$path, character(0))
  expect_equal(y$file$path, character(0))
})

tar_test("error = \"continue\" means continue on error", {
  skip_cran()
  x <- target_init("x", expr = quote(stop(123)), error = "continue")
  y <- target_init("y", expr = quote(stop(456)), error = "continue")
  pipeline <- pipeline_init(list(x, y))
  suppressWarnings(
    expect_warning(
      suppressMessages(local_init(pipeline)$run()),
      class = "tar_condition_run"
    )
  )
  expect_equal(x$file$path, character(0))
  expect_equal(y$file$path, character(0))
  meta <- meta_init()$database$read_condensed_data()
  expect_true(all(nzchar(meta$error)))
  expect_equal(x$file$path, character(0))
  expect_equal(y$file$path, character(0))
})

tar_test("error = \"abridge\" means do not schedule new targets", {
  skip_cran()
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
  skip_cran()
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
  skip_cran()
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
  skip_cran()
  tar_option_set(envir = tmpenv(a = "123"))
  local_init(pipeline_init())$start()
  x <- target_init("abc", expr = quote(a), format = "rds", storage = "main")
  pipeline <- pipeline_init(list(x))
  meta1 <- meta_init()
  on.exit(meta1$database$close())
  scheduler <- scheduler_init(pipeline, meta1)
  target_run(x, tar_option_get("envir"), path_store_default())
  expect_false(file.exists(x$file$path))
  expect_true(is.na(x$file$hash))
  meta <- meta_init()
  on.exit(meta$database$close(), add = TRUE)
  lookup_set(meta$depends, "abc", NA_character_)
  target_conclude(x, pipeline, scheduler, meta)
  expect_true(file.exists(x$file$path))
  expect_false(is.na(x$file$hash))
  path <- file.path("_targets", "objects", "abc")
  expect_equal(readRDS(path), "123")
  expect_equal(target_read_value(x)$object, "123")
  target_conclude(x, pipeline, scheduler, meta)
})

tar_test("builder writing from worker", {
  skip_cran()
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
  expect_true(file.exists(x$file$path))
  expect_false(is.na(x$file$hash))
  path <- file.path("_targets", "objects", "abc")
  expect_equal(readRDS(path), "123")
  expect_equal(target_read_value(x)$object, "123")
  pipeline <- pipeline_init(list(x))
  meta1 <- meta_init()
  on.exit(meta1$database$close())
  scheduler <- scheduler_init(pipeline, meta1)
  meta <- meta_init()
  on.exit(meta$database$close(), add = TRUE)
  lookup_set(meta$depends, "abc", NA_character_)
  target_conclude(x, pipeline, scheduler, meta)
})

tar_test("retrieval = \"none\"", {
  skip_cran()
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
  skip_cran()
  tar_script(tar_target(x, 1, storage = "none", memory = "persistent"))
  expect_error(tar_make(callr_function = NULL), class = "tar_condition_run")
})

tar_test("storage = \"none\" ignores return value but tracks file", {
  skip_cran()
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
  expect_equal(tar_progress(x)$progress, "completed")
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
  expect_equal(tar_progress(x)$progress, "completed")
  expect_equal(tar_progress(y)$progress, "completed")
  expect_false(any(hash == tar_meta(x, data)$data))
})

tar_test("file target writing from main", {
  skip_cran()
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
  expect_true(file.exists(x$file$path))
  expect_false(is.na(x$file$hash))
  pipeline <- pipeline_init(list(x))
  meta1 <- meta_init()
  on.exit(meta1$database$close())
  scheduler <- scheduler_init(pipeline, meta1)
  meta <- meta_init()
  on.exit(meta$database$close())
  lookup_set(meta$depends, "abc", NA_character_)
  target_conclude(x, pipeline, scheduler, meta)
})

tar_test("file target has illegal path", {
  skip_cran()
  x <- target_init(
    name = "abc",
    expr = quote("a*b"),
    format = "file"
  )
  local <- local_init(pipeline_init(list(x)))
  expect_error(local$run(), class = "tar_condition_run")
})

tar_test("file target has missing path value", {
  skip_cran()
  x <- target_init(
    name = "abc",
    expr = quote(NA_character_),
    format = "file"
  )
  local <- local_init(pipeline_init(list(x)))
  expect_error(local$run(), class = "tar_condition_run")
})

tar_test("file target is missing at path", {
  skip_cran()
  x <- target_init(
    name = "abc",
    expr = quote("nope"),
    format = "file",
    deployment = "main"
  )
  local <- local_init(pipeline_init(list(x)))
  expect_error(local$run(), class = "tar_condition_run")
})

tar_test("file target writing with worker deployment", {
  skip_cran()
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
  # Not actually on a worker, even though deployment is "worker"
  # by default and storage is "worker".
  expect_false(is.null(x$value))
  expect_true(file.exists(x$file$path))
  expect_false(is.na(x$file$hash))
  pipeline <- pipeline_init(list(x))
  meta <- meta_init()
  on.exit(meta$database$close())
  scheduler <- scheduler_init(pipeline, meta = meta)
  lookup_set(meta$depends, "abc", NA_character_)
  target_conclude(x, pipeline, scheduler, meta)
})

tar_test("file target writing when actually on worker", {
  skip_cran()
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
  target_run_worker(
    x,
    tar_option_get("envir"),
    path_store_default(),
    envvars = data.frame(),
    options = tar_options$export(),
    fun = "tar_make"
  )
  # On an actual worker, we assume the value was saved to storage.
  expect_null(x$value)
  expect_true(file.exists(x$file$path))
  expect_false(is.na(x$file$hash))
  pipeline <- pipeline_init(list(x))
  meta <- meta_init()
  on.exit(meta$database$close())
  scheduler <- scheduler_init(pipeline, meta = meta)
  lookup_set(meta$depends, "abc", NA_character_)
  target_conclude(x, pipeline, scheduler, meta)
})

tar_test("value kept if storage is local", {
  skip_cran()
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
  expect_equal(sort(counter_get_names(progress$dispatched)), character(0))
  expect_equal(sort(counter_get_names(progress$completed)), character(0))
  expect_equal(sort(counter_get_names(progress$skipped)), character(0))
  expect_equal(sort(counter_get_names(progress$canceled)), character(0))
  expect_equal(sort(counter_get_names(progress$errored)), character(0))
  local$run()
  progress <- local$scheduler$progress
  expect_equal(sort(counter_get_names(progress$queued)), character(0))
  expect_equal(sort(counter_get_names(progress$dispatched)), character(0))
  expect_equal(
    sort(counter_get_names(progress$completed)),
    sort(pipeline_get_names(pipeline))
  )
  expect_equal(sort(counter_get_names(progress$skipped)), character(0))
  expect_equal(sort(counter_get_names(progress$canceled)), character(0))
  expect_equal(sort(counter_get_names(progress$errored)), character(0))
})

tar_test("builders load their packages", {
  skip_cran()
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
  skip_cran()
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
  expect_equal(grepl("error_data1", meta$error), meta$name == "data1")
  expect_equal(grepl("error_data2", meta$error), meta$name == "data2")
})

tar_test("target_needs_worker(builder)", {
  skip_cran()
  x <- tar_target(y, rep(x, 2), deployment = "worker")
  expect_true(target_needs_worker(x))
  x <- tar_target(y, rep(x, 2), deployment = "main")
  expect_false(target_needs_worker(x))
})

tar_test("bootstrap builder for shortcut", {
  skip_cran()
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
  expect_equal(progress$progress, "completed")
})

tar_test("informative error when bootstrap fails", {
  skip_cran()
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
    class = "tar_condition_run"
  )
})

tar_test("validate with nonmissing file and value", {
  skip_cran()
  x <- target_init(name = "abc", expr = quote(1L + 1L))
  x$value <- value_init(123)
  file <- x$file
  file$path <- tempfile()
  expect_silent(tmp <- target_validate(x))
})

tar_test("convert dep loading errors into runtime errors", {
  skip_cran()
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
  expect_equal(tar_progress(x3)$progress, "completed")
  expect_equal(tar_objects(), "x3")
  expect_true(tar_read(x3))
})

tar_test("error = \"null\" without branching", {
  skip_cran()
  tar_script({
    library(targets)
    f <- function(x) {
      stopifnot(x < 1.5)
      x
    }
    list(
      tar_target(x, 2),
      tar_target(y, f(x), error = "null"),
      tar_target(z, y)
    )
  })
  tar_make(callr_function = NULL)
  expect_equal(tar_progress(x)$progress, "completed")
  expect_equal(tar_progress(y)$progress, "errored")
  expect_equal(tar_progress(z)$progress, "completed")
  expect_equal(tar_read(x), 2L)
  expect_null(unname(tar_read(y)))
  expect_null(unname(tar_read(z)))
  expect_equal(
    sort(tar_objects()),
    sort(c("x", "z"))
  )
  expect_equal(
    list.files(path_objects_dir(path_store_default())),
    sort(c("x", "z"))
  )
  expect_null(tar_read(y))
})

tar_test("error = \"null\" with branching", {
  skip_cran()
  tar_script({
    library(targets)
    tar_option_set(memory = "transient")
    f <- function(x) {
      stopifnot(x < 1.5)
      x
    }
    list(
      tar_target(x, seq_len(2)),
      tar_target(y, f(x), pattern = map(x), error = "null"),
      tar_target(z, y)
    )
  })
  tar_make(callr_function = NULL)
  branches <- tar_meta(y)$children[[1]]
  expect_equal(tar_progress(x)$progress, "completed")
  expect_equal(tar_progress(y)$progress, "errored")
  expect_equal(tar_progress(z)$progress, "completed")
  progress <- tar_progress()
  value <- progress$progress[progress$name == branches[1]]
  expect_equal(value, "completed")
  value <- progress$progress[progress$name == branches[2]]
  expect_equal(value, "errored")
  expect_equal(tar_read(x), seq_len(2))
  expect_equal(unname(tar_read(y)), 1L)
  expect_equal(unname(tar_read(z)), 1L)
  expect_equal(tar_read_raw(branches[1]), 1)
  expect_null(tar_read_raw(branches[2]))
  expect_equal(tar_errored(), sort(c("y", branches[2])))
  out <- tar_outdated(branches = TRUE, callr_function = NULL)
  expect_equal(sort(out), sort(c("y", "z", branches[2])))
  meta <- tar_meta()
  expect_equal(meta$data[meta$name == branches[2]], "error")
  tar_make(callr_function = NULL)
  expect_equal(tar_progress(x)$progress, "skipped")
  expect_equal(tar_progress(y)$progress, "errored")
  expect_equal(tar_progress(z)$progress, "skipped")
  progress <- tar_progress()
  value <- progress$progress[progress$name == branches[1]]
  expect_equal(value, "skipped")
  value <- progress$progress[progress$name == branches[2]]
  expect_equal(value, "errored")
})

tar_test("error = \"trim\" on a stem", {
  skip_cran()
  tar_script({
    tar_option_set(error = "trim")
    list(
      tar_target(index, seq_len(2L)),
      tar_target(a, stop(paste(as.character(index), collapse = ""))),
      tar_target(b, a),
      tar_target(b2, b, pattern = map(b)),
      tar_target(c, index),
      tar_target(c2, c),
      tar_target(d, index, pattern = map(index)),
      tar_target(d2, d, pattern = map(d))
    )
  })
  tar_make(callr_function = NULL)
  names <- c(
    "a",
    "c",
    "c2",
    "d",
    tar_meta(d)$children[[1L]],
    "d2",
    tar_meta(d2)$children[[1L]],
    "index"
  )
  progress <- tar_progress()
  expect_equal(nrow(progress), 10L)
  expect_false(any(c("b", "b2") %in% progress$name))
  progress <- tar_progress(names = tidyselect::any_of(names))
  expect_equal(progress$name, names)
  expect_equal(progress$progress, c("errored", rep("completed", 9L)))
  tar_make(callr_function = NULL)
  progress <- tar_progress()
  expect_equal(nrow(progress), 10L)
  expect_false(any(c("b", "b2") %in% progress$name))
  progress <- tar_progress(names = tidyselect::any_of(names))
  expect_equal(progress$name, names)
  expect_equal(progress$progress, c("errored", rep("skipped", 9L)))
})

tar_test("error = \"trim\" on a dynamic branch", {
  skip_cran()
  tar_script({
    tar_option_set(error = "trim")
    list(
      tar_target(index, seq_len(2L)),
      tar_target(a, if (index < 2L) stop(), pattern = map(index)),
      tar_target(b, a, pattern = map(a)),
      tar_target(b2, b),
      tar_target(c, index),
      tar_target(c2, c),
      tar_target(d, index, pattern = map(index)),
      tar_target(d2, d, pattern = map(d))
    )
  })
  tar_make(callr_function = NULL)
  names <- c(
    "c",
    "c2",
    "d",
    tar_meta(d)$children[[1L]],
    "d2",
    tar_meta(d2)$children[[1L]],
    "index"
  )
  progress <- tar_progress()
  expect_equal(nrow(progress), 11L)
  expect_false(any(c("b", "b2") %in% progress$name))
  progress <- tar_progress(names = tidyselect::starts_with("a"))
  expect_equal(unique(progress$progress), "errored")
  progress <- tar_progress(names = tidyselect::any_of(names))
  expect_equal(unique(progress$progress), "completed")
  tar_make(callr_function = NULL)
  progress <- tar_progress()
  expect_equal(nrow(progress), 11L)
  expect_false(any(c("b", "b2") %in% progress$name))
  progress <- tar_progress(names = tidyselect::starts_with("a"))
  expect_equal(unique(progress$progress), "errored")
  progress <- tar_progress(names = tidyselect::any_of(names))
  expect_equal(unique(progress$progress), "skipped")
})

tar_test("error = \"trim\" with a long chain of reverse dependencies", {
  tar_script({
    tar_option_set(
      error = "trim",
      controller = crew::crew_controller_sequential()
    )
    list(
      tar_target(erroring_target, stop()),
      tar_target(downstream_1, erroring_target),
      tar_target(downstream_2, downstream_1),
      tar_target(downstream_3, downstream_2),
      tar_target(dynamic, downstream_3, pattern = map(downstream_3)),
      tar_target(aggregate, dynamic)
    )
  })
  tar_make(callr_function = NULL)
  progress <- tar_progress()
  expect_equal(progress$name, "erroring_target")
  expect_equal(progress$progress, "errored")
})

tar_test("error = \"trim\", long chain of revdeps, dynamic branching", {
  tar_script({
    tar_option_set(
      error = "trim",
      controller = crew::crew_controller_sequential()
    )
    list(
      tar_target(a, seq_len(2)),
      tar_target(erroring_target, stop(), pattern = map(a)),
      tar_target(downstream_1, erroring_target, pattern = map(erroring_target)),
      tar_target(downstream_2, downstream_1, pattern = map(downstream_1)),
      tar_target(downstream_3, downstream_2),
      tar_target(dynamic, downstream_3, pattern = map(downstream_3)),
      tar_target(aggregate, dynamic)
    )
  })
  tar_make(callr_function = NULL)
  progress <- tar_progress()
  expect_equal(nrow(progress), 3L)
  expect_equal(
    unique(progress$progress[grepl("^erroring_", progress$name)]),
    "errored"
  )
  expect_equal(progress$progress[progress$name == "a"], "completed")
})

tar_test("capture storage warnings", {
  skip_cran()
  tar_script(
    list(
      tar_target(
        x,
        {
          warning("run_warning")
          123L
        },
        format = tar_format(
          read = function(path) {
            readRDS(path)
          },
          write = function(object, path) {
            warning("storage_warning")
            saveRDS(object, path)
          }
        )
      )
    )
  )
  suppressWarnings(
    expect_warning(
      tar_make(callr_function = NULL),
      class = "tar_condition_run"
    )
  )
  expect_equal(tar_read(x), 123L)
  expect_equal(tar_meta(x)$warnings, "run_warning. storage_warning")
})
