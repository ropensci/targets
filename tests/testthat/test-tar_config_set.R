tar_test("tar_config_set() with script", {
  expect_false(file.exists("_targets.yaml"))
  expect_equal(tar_config_get("script"), path_script_default())
  path <- tempfile()
  tar_config_set(script = path)
  expect_equal(tar_config_get("script"), path)
  expect_true(file.exists("_targets.yaml"))
  tar_config_set()
  expect_equal(tar_config_get("script"), path)
  expect_true(file.exists("_targets.yaml"))
  unlink("_targets.yaml")
  expect_equal(tar_config_get("script"), path_script_default())
})

tar_test("tar_config_set() with store", {
  expect_false(file.exists("_targets.yaml"))
  expect_equal(tar_config_get("store"), path_store_default())
  path <- tempfile()
  tar_config_set(store = path)
  expect_equal(tar_config_get("store"), path)
  expect_true(file.exists("_targets.yaml"))
  tar_config_set()
  expect_equal(tar_config_get("store"), path)
  expect_true(file.exists("_targets.yaml"))
  unlink("_targets.yaml")
  expect_equal(tar_config_get("store"), path_store_default())
})

tar_test("_targets.yaml is locked during the pipeline then unlocked after", {
  tar_script({
    list(
      tar_target(a, "store: _targets2"),
      tar_target(x, writeLines(a, "_targets.yaml")),
      tar_target(y, x)
    )
  })
  tar_make(callr_function = NULL)
  expect_false(file.exists("_targets2"))
  expect_true(file.exists("_targets"))
  expect_equal(tar_config_get("store"), "_targets2")
  tar_config_set(store = "_targets")
  expect_equal(tar_config_get("store"), "_targets")
  expect_equal(tar_outdated(callr_function = NULL), character(0))
  tar_make(callr_function = NULL)
  expect_equal(nrow(tar_progress()), 0L)
})

tar_test("same with external process", {
  tar_script({
    list(
      tar_target(a, "store: _targets2"),
      tar_target(x, writeLines(a, "_targets.yaml")),
      tar_target(y, x)
    )
  })
  tar_make(reporter = "silent")
  expect_false(file.exists("_targets2"))
  expect_true(file.exists("_targets"))
  expect_equal(tar_config_get("store"), "_targets2")
  tar_config_set(store = "_targets")
  expect_equal(tar_config_get("store"), "_targets")
  expect_equal(tar_outdated(callr_function = NULL), character(0))
  tar_make(callr_function = NULL)
  expect_equal(nrow(tar_progress()), 0L)
})

tar_test("tar_config_set() can configure the script and the store", {
  skip_on_cran()
  tar_config_set(script = "example/script.R", store = "example/store")
  tar_script(tar_target(x, 1L))
  tar_make(reporter = "silent")
  expect_equal(tar_read(x), 1L)
  expect_true(file.exists("example/script.R"))
  expect_true(file.exists("example/store"))
  expect_false(file.exists(path_script_default()))
  expect_false(file.exists(path_store_default()))
})

tar_test("tar_config_set() alternative configuration file", {
  skip_on_cran()
  tar_config_set(config = "_targets.yaml")
  on.exit(tar_config_set(config = "_targets.yaml"))
  config <- tempfile()
  script <- tempfile()
  expect_false(file.exists("_targets.yaml"))
  expect_equal(tar_config_get("script"), path_script_default())
  expect_equal(tar_config_get("config"), "_targets.yaml")
  tar_config_set(config = config)
  expect_equal(tar_config_get("config"), config)
  expect_equal(tar_config_get("script"), path_script_default())
  tar_config_set(script = script)
  expect_false(file.exists("_targets.yaml"))
  expect_true(file.exists(config))
  expect_equal(tar_config_get("script"), script)
  expect_equal(readLines(config), paste("script:", script))
  tar_config_set(config = "_targets.yaml")
  expect_equal(tar_config_get("script"), path_script_default())
  expect_false(file.exists("_targets.yaml"))
})
