tar_test("tar_store() outside a pipeline", {
  skip_cran()
  expect_warning(tar_store(), class = "tar_condition_deprecate")
  expect_equal(suppressWarnings(tar_store()), tar_config_get("store"))
  tar_config_set(store = "example_store")
  expect_equal(suppressWarnings(tar_store()), tar_config_get("store"))
})

tar_test("tar_store() inside a pipeline", {
  skip_cran()
  tar_script(tar_target(x, suppressWarnings(tar_store())), ask = FALSE)
  store <- "example_store"
  tar_make(store = store, callr_function = NULL)
  expect_equal(tar_read(x, store = store), store)
})
