tar_test("dynamic files work", {
  envir <- new.env(parent = environment())
  x <- target_init(
    name = "abc",
    expr = quote(f()),
    format = "file"
  )
  envir$f <- function() {
    file <- tempfile()
    writeLines("lines", con = file)
    file
  }
  builder_update_build(x, envir)
  expect_equal(x$store$file$path, character(0))
  expect_true(is.na(x$store$file$hash))
  builder_update_paths(x, path_store_default())
  expect_true(file.exists(x$store$file$path))
  expect_true(nzchar(x$store$file$path))
  expect_false(is.na(x$store$file$hash))
  expect_silent(store_validate(x$store))
  expect_true(file.exists(x$store$file$path))
  expect_false(is.na(x$store$file$hash))
  builder_update_object(x)
  expect_false(file.exists(file.path("_targets", "objects", "abc")))
  expect_equal(readLines(x$value$object), "lines")
  out <- target_read_value(x)$object
  exp <- x$store$file$path
  expect_equal(out, exp)
})

tar_test("dynamic files must return characters", {
  x <- target_init(
    name = "abc",
    expr = quote(list(list("illegal"))),
    format = "file"
  )
  pipeline <- pipeline_init(list(x))
  local <- local_init(pipeline = pipeline)
  expect_error(local$run(), class = "tar_condition_run")
})

tar_test("handle dynamic file errors properly", {
  x <- target_init(
    name = "abc",
    expr = quote(stop("message123")),
    format = "file"
  )
  pipeline <- pipeline_init(list(x))
  local <- local_init(pipeline = pipeline)
  expect_error(local$run(), class = "tar_condition_run")
  expect_equal(tar_meta(abc, error)$error, "message123")
})

tar_test("inherits from tar_external", {
  store <- tar_target(x, "x_value", format = "file")$store
  expect_true(inherits(store, "tar_external"))
})

tar_test("store_row_path()", {
  store <- tar_target(x, "x_value", format = "file")$store
  store$file$path <- "path"
  expect_equal(store_row_path(store), "path")
})

tar_test("store_path_from_record()", {
  store <- tar_target(x, "x_value", format = "file")$store
  record <- record_init(path = "path", format = "file")
  expect_equal(
    store_path_from_record(store, record, path_store_default()),
    "path"
  )
})

tar_test("files can be empty (#728)", {
  tar_script(
    list(
      tar_target(x, character(0), format = "file"),
      tar_target(y, x)
    )
  )
  tar_make(callr_function = NULL)
  expect_equal(tar_outdated(callr_function = NULL), character(0))
  expect_equal(tar_read(x), character(0))
  expect_equal(tar_read(y), character(0))
  expect_true(is.na(tar_meta(x)$path))
  tar_script(
    list(
      tar_target(x, (character(0)), format = "file"),
      tar_target(y, x)
    )
  )
  expect_equal(
    sort(tar_outdated(callr_function = NULL)),
    c("x", "y")
  )
})

tar_test("file and NULL", {
  skip_cran()
  tar_script(
    list(
      tar_target(x, NULL, format = "file", memory = "persistent"),
      tar_target(y, x, memory = "persistent")
    )
  )
  tar_make(callr_function = NULL)
  expect_equal(tar_read(x), character(0))
  expect_equal(tar_read(y), character(0))
})
