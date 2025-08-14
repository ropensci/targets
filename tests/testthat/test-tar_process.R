tar_test("tar_process() with no args", {
  tar_script(tar_target(x, 1))
  tar_make(callr_function = NULL)
  out <- tar_process()
  expect_equal(sort(colnames(out)), sort(c("name", "value")))
  names <- c("pid", "created", "version_r", "version_targets")
  expect_true(all(names %in% out$name))
  expect_true(all(nzchar(out$value)))
  expect_true(is.finite(as.integer(out$value[out$name == "pid"])))
})

tar_test("tar_process() with names", {
  tar_script(tar_target(x, 1))
  tar_make(callr_function = NULL)
  out <- tar_process(pid)
  expect_equal(out$name, "pid")
  expect_equal(as.integer(out$value), Sys.getpid())
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
  expect_true(is.data.frame(tar_process(store = "example/store")))
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
