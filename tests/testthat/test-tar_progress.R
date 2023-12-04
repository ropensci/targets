tar_test("tar_progress() with defaults", {
  for (result in c("completed", "skipped")) {
    pipeline <- pipeline_init(list(target_init("x", quote(1))))
    local_init(pipeline = pipeline)$run()
    out <- tar_progress()
    expect_equal(dim(out), c(1L, 2L))
    expect_equal(out$name, "x")
    expect_equal(out$progress, result)
  }
})

tar_test("tar_progress() with fields = NULL", {
  pipeline <- pipeline_init(list(target_init("x", quote(1))))
  local_init(pipeline = pipeline)$run()
  out <- tar_progress(fields = NULL)
  expect_equal(dim(out), c(1L, 5L))
  expect_equal(out$name, "x")
  expect_equal(out$type, "stem")
  expect_equal(out$parent, "x")
  expect_equal(out$branches, 0L)
  expect_equal(out$progress, "completed")
})

tar_test("tar_progress() tidyselect", {
  pipeline <- pipeline_init(list(target_init("x", quote(1))))
  local_init(pipeline = pipeline)$run()
  out <- tar_progress(fields = type)
  expect_equal(dim(out), c(1L, 2L))
  expect_equal(out$name, "x")
  expect_equal(out$type, "stem")
})

tar_test("tar_progress() with target selection", {
  tar_script({
    envir <- new.env(parent = baseenv())
    tar_option_set(envir = envir)
    list(
      tar_target(x, seq_len(2)),
      tar_target(y, 2 * x, pattern = map(x))
    )
  })
  tar_make(callr_function = NULL)
  out <- tar_progress()
  expect_equal(nrow(out), 4L)
  out <- tar_progress(c("y", "x"))
  expect_equal(out$name, c("y", "x"))
  out <- tar_progress(c("x", "y"))
  expect_equal(out$name, c("x", "y"))
})

tar_test("custom script and store args", {
  skip_cran()
  expect_equal(tar_config_get("script"), path_script_default())
  expect_equal(tar_config_get("store"), path_store_default())
  tar_script({
    list(
      tar_target(w, letters)
    )
  }, script = "example/script.R")
  tar_make(
    callr_function = NULL,
    script = "example/script.R",
    store = "example/store"
  )
  expect_true(is.data.frame(tar_progress(store = "example/store")))
  expect_false(file.exists("_targets.yaml"))
  expect_equal(tar_config_get("script"), path_script_default())
  expect_equal(tar_config_get("store"), path_store_default())
  expect_false(file.exists(path_script_default()))
  expect_false(file.exists(path_store_default()))
  expect_true(file.exists("example/script.R"))
  expect_true(file.exists("example/store"))
  expect_true(file.exists("example/store/meta/meta"))
  expect_true(file.exists("example/store/objects/w"))
  tar_config_set(script = "x")
  expect_equal(tar_config_get("script"), "x")
  expect_true(file.exists("_targets.yaml"))
})
