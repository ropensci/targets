tar_test("tar_config_unset()", {
  store <- tar_config_get("store")
  script <- tar_config_get("script")
  workers <- tar_config_get("workers")
  tar_config_set(store = "abc", script = "123", workers = workers + 500L)
  expect_equal(tar_config_get("store"), "abc")
  expect_equal(tar_config_get("script"), "123")
  expect_equal(tar_config_get("workers"), workers + 500L)
  tar_config_unset(c("store", "script"))
  expect_equal(tar_config_get("store"), path_store_default())
  expect_equal(tar_config_get("script"), path_script_default())
  expect_equal(tar_config_get("workers"), workers + 500L)
})

tar_test("tar_config_unset() unsets from correct project (arg)", {
  tar_config_set(store = "abc", project = "project1")
  tar_config_set(store = "xyz", project = "project2")
  tar_config_unset("store", project = "project1")
  expect_equal(
    tar_config_get("store", project = "project1"),
    path_store_default()
  )
  expect_equal(tar_config_get("store", project = "project2"), "xyz")
  tar_config_unset("store", project = "project2")
  expect_equal(
    tar_config_get("store", project = "project2"),
    path_store_default()
  )
})

tar_test("tar_config_unset() unsets from correct project (env var)", {
  on.exit(Sys.unsetenv("TAR_PROJECT"))
  Sys.setenv(TAR_PROJECT = "project1")
  tar_config_set(store = "abc")
  Sys.setenv(TAR_PROJECT = "project2")
  tar_config_set(store = "xyz")
  Sys.setenv(TAR_PROJECT = "project1")
  tar_config_unset("store")
  expect_equal(tar_config_get("store"), path_store_default())
  Sys.setenv(TAR_PROJECT = "project2")
  expect_equal(tar_config_get("store"), "xyz")
  tar_config_unset("store")
  expect_equal(tar_config_get("store"), path_store_default())
})

tar_test("tar_config_unset() converts to multi-project format", {
  writeLines("store: abc\nscript: x123", "_targets.yaml")
  expect_warning(
    tar_config_unset("store"),
    class = "tar_condition_deprecate"
  )
  expect_equal(tar_config_get("script"), "x123")
  out <- yaml::read_yaml("_targets.yaml")
  expect_equal(out, list(main = list(script = "x123")))
})
