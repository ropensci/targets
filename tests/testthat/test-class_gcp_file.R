tar_test("gcp_file packages", {
  target <- tar_target(
    x,
    "x_value",
    format = "file",
    repository = "gcp"
  )
  expect_equal(
    sort(store_get_packages(target$store)),
    c("googleCloudStorageR")
  )
})

tar_test("inherits from tar_external", {
  store <- tar_target(
    x,
    "x_value",
    format = "file",
    repository = "gcp"
  )$store
  expect_true(inherits(store, "tar_external"))
})

tar_test("store_row_path()", {
  target <- tar_target(
    x,
    "x_value",
    format = "file",
    repository = "gcp"
  )
  store <- target$store
  file <- target$file
  file$path <- "path"
  expect_equal(store_row_path(store, file), "path")
})

tar_test("store_path_from_name()", {
  store <- tar_target(
    x,
    "x_value",
    format = "file",
    repository = "gcp"
  )$store
  out <- store_path_from_name(
    store,
    format = store$format,
    name = "x",
    path = "path",
    path_store = path_store_default()
  )
  expect_equal(out, "path")
})

tar_test("validate gcp file", {
  skip_if_not_installed("googleCloudStorageR")
  tar_script(
    list(
      tar_target(
        x,
        "x_value",
        format = "file",
        repository = "gcp"
      )
    )
  )
  expect_silent(tar_validate(callr_function = NULL))
})
