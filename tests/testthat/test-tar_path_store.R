tar_test("tar_path_store() outside a pipeline", {
  skip_cran()
  expect_equal(tar_path_store(), tar_config_get("store"))
  tar_config_set(store = "example_store")
  expect_equal(tar_path_store(), tar_config_get("store"))
})

tar_test("tar_path_store() inside a pipeline", {
  skip_cran()
  tar_script(tar_target(x, tar_path_store()), ask = FALSE)
  store <- "example_store"
  tar_make(store = store, callr_function = NULL)
  expect_equal(tar_read(x, store = store), store)
})
