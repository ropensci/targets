tar_test("target", {
  x <- runtime_new()
  x$target <- tar_target(x, 1)
  expect_true(is.environment(x$target))
  expect_silent(runtime_validate(x))
})

tar_test("frames", {
  x <- runtime_new()
  expect_null(x$frames)
  x$frames <- frames_init()
  expect_true(is.environment(x$frames))
  expect_silent(runtime_validate(x))
})

tar_test("interactive", {
  x <- runtime_new()
  expect_null(x$interactive)
  x$interactive <- TRUE
  expect_true(x$interactive)
  expect_silent(runtime_validate(x))
})

tar_test("script", {
  x <- runtime_new()
  expect_null(x$script)
  x$script <- "script"
  expect_equal(x$script, "script")
  expect_silent(runtime_validate(x))
})

tar_test("store", {
  x <- runtime_new()
  expect_null(x$store)
  x$store <- "store"
  expect_equal(x$store, "store")
  expect_silent(runtime_validate(x))
})

tar_test("working_directory", {
  x <- runtime_new()
  expect_null(x$working_directory)
  x$working_directory <- "working_directory"
  expect_equal(x$working_directory, "working_directory")
  expect_silent(runtime_validate(x))
})

tar_test("fun", {
  x <- runtime_new()
  expect_null(x$fun)
  x$fun <- "tar_make"
  expect_equal(x$fun, "tar_make")
  expect_silent(runtime_validate(x))
})

tar_test("active", {
  x <- runtime_new()
  expect_null(x$active)
  x$active <- TRUE
  expect_true(x$active)
  expect_silent(runtime_validate(x))
})

tar_test("gcp_auth", {
  x <- runtime_new()
  expect_null(x$gcp_auth)
  x$gcp_auth <- TRUE
  expect_true(x$gcp_auth)
  expect_silent(runtime_validate(x))
})

tar_test("file_exist", {
  x <- runtime_new()
  expect_null(x$file_exist)
  x$file_exist <- counter_init()
  expect_true(is.environment(x$file_exist))
  expect_silent(runtime_validate(x))
})

tar_test("file_info", {
  x <- runtime_new()
  expect_null(x$file_info)
  tmp <- tempfile()
  file.create(tmp)
  x$file_info <- file.info(tmp, extra_cols = FALSE)
  expect_true(is.data.frame(x$file_info))
  expect_silent(runtime_validate(x))
})

tar_test("traceback", {
  x <- runtime_new()
  expect_null(x$traceback)
  x$traceback <- "calls"
  expect_equal(x$traceback, "calls")
  expect_silent(runtime_validate(x))
})

tar_test("pid_parent", {
  x <- runtime_new()
  expect_null(x$pid_parent)
  x$pid_parent <- 1L
  expect_equal(x$pid_parent, 1L)
  expect_silent(runtime_validate(x))
  x$pid_parent <- "bad"
  expect_error(runtime_validate(x), class = "tar_condition_validate")
})

tar_test("validate null fields", {
  x <- runtime_new()
  expect_silent(runtime_validate(x))
})

tar_test("validate non-null runtime", {
  x <- runtime_new(
    target = tar_target(x, 1),
    frames = frames_init(),
    interactive = FALSE
  )
  expect_silent(runtime_validate(x))
})

tar_test("validate non-null file_systems", {
  x <- runtime_new(
    target = tar_target(x, 1),
    frames = frames_init(),
    interactive = FALSE,
    file_systems = "x"
  )
  expect_silent(runtime_validate(x))
})

tar_test("validate non-null trust_timestamps_store", {
  x <- runtime_new(
    target = tar_target(x, 1),
    frames = frames_init(),
    interactive = FALSE,
    trust_timestamps_store = TRUE
  )
  expect_silent(runtime_validate(x))
})

tar_test("invalidate bad runtime", {
  x <- runtime_new(target = 1, frames = frames_init())
  expect_error(runtime_validate(x), class = "tar_condition_validate")
})

tar_test("invalidate bad interactive", {
  x <- runtime_new(interactive = letters)
  expect_error(runtime_validate(x), class = "tar_condition_validate")
})

tar_test("invalidate bad interactive", {
  x <- runtime_new(interactive = letters)
  expect_error(runtime_validate(x), class = "tar_condition_validate")
})

tar_test("validate non-null script", {
  x <- runtime_new()
  x$script <- "script"
  expect_silent(runtime_validate(x))
})

tar_test("validate non-null progress_bar", {
  x <- runtime_new()
  x$progress_bar <- TRUE
  expect_silent(runtime_validate(x))
})

tar_test("detect bad script", {
  x <- runtime_new()
  x$script <- FALSE
  expect_error(runtime_validate(x), class = "tar_condition_validate")
})

tar_test("validate non-null store", {
  x <- runtime_new()
  x$store <- "store"
  expect_silent(runtime_validate(x))
})

tar_test("runtime reset", {
  x <- runtime_new()
  x$store <- "store"
  x$script <- "script"
  expect_equal(x$store, "store")
  expect_equal(x$script, "script")
  runtime_reset(x)
  expect_null(x$store)
  expect_null(x$script)
})

tar_test("runtime_set_file_info()", {
  x <- runtime_new()
  store <- path_store_default()
  dir_create(path_objects_dir(store))
  writeLines("x", path_objects(store, "x"))
  writeLines("y", path_objects(store, "y"))
  runtime_set_file_info(x, store, c("x", "y"))
  expect_true(is.character(x$file_info$path))
  for (field in c("size", "mtime_numeric")) {
    expect_true(is.numeric(x$file_info[[field]]))
  }
  expect_true(is.logical(x$file_info[["trust_timestamps"]]))
  for (field in c("file_exist")) {
    expect_true(is.environment(x[[field]]))
    expect_silent(counter_validate(x[[field]]))
    expect_equal(x[[field]]$count, 2L)
    expect_equal(as.logical(as.list(x[[field]]$envir)), c(TRUE, TRUE))
    expect_equal(
      sort(names(as.list(x[[field]]$envir))),
      sort(c(path_objects(store, "x"), path_objects(store, "y")))
    )
  }
})

tar_test("detect bad store", {
  x <- runtime_new()
  x$store <- FALSE
  expect_error(runtime_validate(x), class = "tar_condition_validate")
})

tar_test("validate non-null fun", {
  x <- runtime_new()
  x$fun <- "tar_make"
  expect_silent(runtime_validate(x))
})

tar_test("detect bad fun", {
  x <- runtime_new()
  x$fun <- ""
  expect_error(runtime_validate(x), class = "tar_condition_validate")
})

tar_test("runtime inventories", {
  x <- runtime_new()
  expect_silent(runtime_validate(x))
  x$inventories <- list()
  expect_silent(runtime_validate(x))
  x$inventories$aws <- inventory_init()
  expect_silent(runtime_validate(x))
  x$inventories <- ""
  expect_error(runtime_validate(x), class = "tar_condition_validate")
})

tar_test("runtime_increment_targets_run()", {
  x <- runtime_new()
  expect_null(x$number_targets_run)
  runtime_increment_targets_run(x)
  expect_equal(x$number_targets_run, 1L)
  runtime_increment_targets_run(x)
  expect_equal(x$number_targets_run, 2L)
  expect_silent(runtime_validate(x))
})

tar_test("validate non-null metadata", {
  meta <- meta_init()
  on.exit(meta$database$close())
  x <- runtime_new(meta = meta)
  expect_silent(runtime_validate(x))
})
