tar_test("tar_delete() works", {
  pipeline <- pipeline_init(
    list(
      target_init("y1", quote(1 + 1)),
      target_init("y2", quote(1 + 1)),
      target_init("z", quote(y1 + y2))
    )
  )
  local_init(pipeline = pipeline)$run()
  tar_delete(starts_with("y")) # Only deletes y1 and y2
  files <- list.files(file.path("_targets", "objects"))
  expect_equal(files, "z")
})

tar_test("tar_delete() works with patterns", {
  pipeline <- pipeline_init(
    list(
      target_init("x", quote(seq_len(2))),
      target_init("y", quote(x), pattern = quote(map(x))),
      target_init("z", quote(y), pattern = quote(map(y)))
    )
  )
  local_init(pipeline = pipeline)$run()
  tar_delete(starts_with("y")) # Only deletes y1 and y2
  data <- meta_init()$database$read_data()
  names <- list.files(file.path("_targets", "objects"))
  expect_equal(sum(grepl("^x", names)), 1L)
  expect_equal(sum(grepl("^y", names)), 0L)
  expect_equal(sum(grepl("^z", names)), 2L)
})

tar_test("tar_delete() does not delete file targets", {
  file.create("x")
  pipeline <- pipeline_init(
    list(target_init("x", quote("x"), format = "file"))
  )
  local_init(pipeline = pipeline)$run()
  tar_delete(x)
  expect_true(file.exists("x"))
})

tar_test("custom script and store args", {
  skip_cran()
  expect_equal(tar_config_get("script"), path_script_default())
  expect_equal(tar_config_get("store"), path_store_default())
  tar_script(
    {
      list(
        tar_target(w, letters)
      )
    },
    script = "example/script.R"
  )
  tar_make(
    callr_function = NULL,
    script = "example/script.R",
    store = "example/store"
  )
  expect_true(file.exists("example/store/objects/w"))
  tar_delete(w, store = "example/store")
  expect_false(file.exists("example/store/objects/w"))
  expect_false(file.exists("_targets.yaml"))
  expect_equal(tar_config_get("script"), path_script_default())
  expect_equal(tar_config_get("store"), path_store_default())
  expect_false(file.exists(path_script_default()))
  expect_false(file.exists(path_store_default()))
  expect_true(file.exists("example/script.R"))
  expect_true(file.exists("example/store"))
  expect_true(file.exists("example/store/meta/meta"))
  tar_config_set(script = "x")
  expect_equal(tar_config_get("script"), "x")
  expect_true(file.exists("_targets.yaml"))
})
