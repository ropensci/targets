tar_test("tar_path_target() outside a pipeline with no arguments", {
  expect_true(is.character(tar_path_target()))
  expect_true(is.na(tar_path_target()))
  expect_equal(length(tar_path_target()), 1L)
  expect_equal(tar_path_target(default = "x"), "x")
})

tar_test("tar_path_target() with a name arg", {
  expect_equal(tar_path_target(x), file.path("_targets", "objects", "x"))
})

tar_test("tar_path_target() inside a pipeline", {
  x <- target_init("x", quote(targets::tar_path_target()))
  tar_runtime$store <- path_store_default()
  on.exit(tar_runtime$store <- NULL)
  pipeline <- pipeline_init(list(x))
  local_init(pipeline)$run()
  path <- file.path("_targets", "objects", "x")
  expect_equal(target_read_value(x, pipeline)$object, path)
})

tar_test("custom script and store args", {
  skip_cran()
  expect_equal(tar_config_get("script"), path_script_default())
  expect_equal(tar_config_get("store"), path_store_default())
  expect_false(file.exists("example/store"))
  out <- tar_path_target(x, store = "example/store")
  expect_equal(out, "example/store/objects/x")
  expect_false(file.exists("example/store"))
  expect_false(file.exists("_targets.yaml"))
  expect_equal(tar_config_get("script"), path_script_default())
  expect_equal(tar_config_get("store"), path_store_default())
  expect_false(file.exists(path_script_default()))
  expect_false(file.exists(path_store_default()))
  expect_false(file.exists("example/script.R"))
  tar_config_set(script = "x")
  expect_equal(tar_config_get("script"), "x")
  expect_true(file.exists("_targets.yaml"))
})

tar_test("tar_path_target() idempotently creates dir if create_dir is TRUE", {
  for (index in seq_len(2)) {
    out <- tar_path_target("x", create_dir = TRUE)
    expect_true(file.exists(dirname(out)))
  }
})

tar_test("tar_path_target() does not create dir if create_dir is FALSE", {
  out <- tar_path_target("x", create_dir = FALSE)
  expect_false(file.exists(dirname(out)))
})

tar_test("tar_path_target() returns non-cloud path for non-cloud storage", {
  x <- tar_target(x, 1, format = "parquet")
  on.exit({
    tar_runtime$store <- NULL
    tar_runtime$target <- NULL
  })
  tar_runtime$store <- path_store_default()
  tar_runtime$target <- x
  out <- tar_path_target(create_dir = FALSE)
  expect_false(file.exists(dirname(out)))
  out <- tar_path_target(create_dir = TRUE)
  expect_true(file.exists(dirname(out)))
  expect_equal(out, path_objects(path_store_default(), "x"))
})

tar_test("tar_path_target() returns stage for cloud formats", {
  skip_cran()
  skip_on_os("windows")
  x <- tar_target(x, 1, format = "parquet", repository = "aws")
  store_update_stage_early(x$store, x$settings$name, path_store_default())
  dir <- dirname(x$store$file$stage)
  unlink(dir, recursive = TRUE)
  on.exit(tar_runtime$target)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  tar_runtime$target <- x
  out <- tar_path_target(create_dir = FALSE)
  expect_false(file.exists(dirname(out)))
  out <- tar_path_target(create_dir = TRUE)
  expect_true(file.exists(dirname(out)))
  expect_equal(dirname(out), file.path(path_scratch_dir_cloud(), "scratch"))
  expect_equal(out, x$store$file$stage)
})

tar_test("tar_path_target() with alternative data store in tar_make()", {
  tar_script(tar_target(x, tar_path_target()))
  store <- "example_store"
  tar_make(callr_function = NULL, store = store)
  expect_equal(
    tar_read(x, store = store),
    path_objects(store, "x")
  )
})
