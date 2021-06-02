tar_test("tar_invalidate() works", {
  pipeline <- pipeline_init(
    list(
      target_init("y1", quote(1 + 1)),
      target_init("y2", quote(1 + 1)),
      target_init("z", quote(y1 + y2))
    )
  )
  local_init(pipeline = pipeline)$run()
  tar_invalidate(starts_with("y")) # Only invalidates y1 and y2
  data <- meta_init()$database$read_data()
  expect_true("z" %in% data$name)
  expect_false(any(c("y1", "y2") %in% data$name))
})


tar_test("tar_invalidate() works with patterns", {
  pipeline <- pipeline_init(
    list(
      target_init("x", quote(seq_len(2))),
      target_init("y", quote(x), pattern = quote(map(x))),
      target_init("z", quote(y), pattern = quote(map(y)))
    )
  )
  local_init(pipeline = pipeline)$run()
  tar_invalidate(starts_with("y")) # Only invalidates y1 and y2
  data <- meta_init()$database$read_data()
  names <- data$name
  expect_equal(sum(grepl("^x", names)), 1L)
  expect_equal(sum(grepl("^y", names)), 0L)
  expect_equal(sum(grepl("^z", names)), 3L)
})

tar_test("custom script and store args", {
  skip_on_cran()
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
  expect_true("w" %in% tar_meta(store = "example/store")$name)
  tar_invalidate(w, store = "example/store")
  expect_false("w" %in% tar_meta(store = "example/store")$name)
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
