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

tar_test("tar_config_unset() unsets from correct project", {
})
