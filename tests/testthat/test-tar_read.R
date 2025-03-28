tar_test("tar_read() on missing builder", {
  pipeline <- pipeline_init(list(target_init("x", quote(1L))))
  local_init(pipeline = pipeline)$run()
  expect_error(tar_read(y), class = "tar_condition_validate")
})

tar_test("tar_read() works on builders", {
  pipeline <- pipeline_init(list(target_init("x", quote(1L))))
  local_init(pipeline = pipeline)$run()
  expect_equal(tar_read(x), 1L)
})

tar_test("tar_read_raw() works", {
  pipeline <- pipeline_init(list(target_init("x", quote(1L))))
  local_init(pipeline = pipeline)$run()
  name <- "x"
  expect_equal(tar_read_raw(name), 1L)
})

tar_test("tar_read() works on a file target", {
  saveRDS("contents", "data_file")
  x <- target_init("x", quote("data_file"), format = "file")
  pipeline <- pipeline_init(list(x))
  local <- local_init(pipeline = pipeline)
  local$run()
  expect_equal(tar_read(x), "data_file")
  expect_equal(list.files(file.path("_targets", "objects")), character(0))
})

tar_test("tar_read() on patterns with vector iteration", {
  pipeline <- pipeline_init(
    list(
      target_init("x", quote(letters[seq_len(4L)])),
      target_init("y", quote(x), pattern = quote(map(x)), iteration = "vector")
    )
  )
  local_init(pipeline = pipeline)$run()
  expect_equal(unname(tar_read(y)), letters[seq_len(4L)])
  out <- tar_read(y, branches = c(2L, 3L))
  expect_equal(unname(out), letters[c(2L, 3L)])
  expect_error(tar_read(y, branches = 99L), class = "tar_condition_validate")
})

tar_test("tar_read() on patterns with list iteration", {
  pipeline <- pipeline_init(
    list(
      target_init("x", quote(letters[seq_len(4L)])),
      target_init("y", quote(x), pattern = quote(map(x)), iteration = "list")
    )
  )
  local_init(pipeline = pipeline)$run()
  expect_equal(unname(tar_read(y)), as.list(letters[seq_len(4L)]))
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
  expect_equal(tar_read(w, store = "example/store"), letters)
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
