tar_test("tar_path() outside a pipeline with no arguments", {
  expect_true(is.character(tar_path()))
  expect_true(is.na(tar_path()))
  expect_equal(length(tar_path()), 1L)
  expect_equal(tar_path(default = "x"), "x")
})

tar_test("tar_path() with a name arg", {
  expect_equal(tar_path(x), file.path("_targets", "objects", "x"))
})

tar_test("tar_path() inside a pipeline", {
  x <- target_init("x", quote(targets::tar_path()))
  pipeline <- pipeline_init(list(x))
  local_init(pipeline)$run()
  path <- file.path("_targets", "objects", "x")
  expect_equal(target_read_value(x, pipeline)$object, path)
})

tar_test("custom script and store args", {
  skip_on_cran()
  expect_equal(tar_config_get("script"), path_script_default())
  expect_equal(tar_config_get("store"), path_store_default())
  expect_false(file.exists("example/store"))
  out <- tar_path(x, store = "example/store")
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

tar_test("tar_path() idempotently creates dir if create_dir is TRUE", {
  for (index in seq_len(2)) {
    out <- tar_path("x", create_dir = TRUE)
    expect_true(file.exists(dirname(out)))
  }
})

tar_test("tar_path() does not create dir if create_dir is FALSE", {
  out <- tar_path("x", create_dir = FALSE)
  expect_false(file.exists(dirname(out)))
})

tar_test("tar_path() returns non-cloud path for non-cloud storage formats", {
  x <- tar_target(x, 1, format = "parquet")
  on.exit({
    tar_runtime$unset_store()
    tar_runtime$unset_target()
  })
  tar_runtime$set_store(path_store_default())
  tar_runtime$set_target(x)
  out <- tar_path(create_dir = FALSE)
  expect_false(file.exists(dirname(out)))
  out <- tar_path(create_dir = TRUE)
  expect_true(file.exists(dirname(out)))
  expect_equal(out, path_objects(path_store_default(), "x"))
})

tar_test("tar_path() returns stage for cloud formats", {
  x <- tar_target(x, 1, format = "aws_parquet")
  store_update_stage_early(x$store, x$settings$name, path_store_default())
  on.exit(tar_runtime$unset_target())
  tar_runtime$set_target(x)
  out <- tar_path(create_dir = FALSE)
  expect_false(file.exists(dirname(out)))
  out <- tar_path(create_dir = TRUE)
  expect_true(file.exists(dirname(out)))
  expect_equal(dirname(out), dirname(path_scratch(path_store_default())))
  expect_equal(out, x$store$file$stage)
})

tar_test("tar_path() with alternative data store in tar_make()", {
  tar_script(tar_target(x, tar_path()))
  store <- "example_store"
  tar_make(callr_function = NULL, store = store)
  expect_equal(
    tar_read(x, store = store),
    path_objects(store, "x")
  )
})
