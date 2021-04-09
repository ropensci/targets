tar_test("tar_config_set()", {
  expect_false(file.exists("_targets.yaml"))
  expect_equal(tar_config_get("store"), "_targets")
  path <- tempfile()
  tar_config_set(store = path)
  expect_equal(tar_config_get("store"), path)
  expect_true(file.exists("_targets.yaml"))
  tar_config_set()
  expect_equal(tar_config_get("store"), path)
  expect_true(file.exists("_targets.yaml"))
  unlink("_targets.yaml")
  expect_equal(tar_config_get("store"), "_targets")
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
