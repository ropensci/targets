tar_test("store_gcp_bucket()", {
  path <- c("bucket=b", "region=r", "key=key_name", "stage=stage_name")
  expect_equal(store_gcp_bucket(path), "b")
})

tar_test("store_gcp_key()", {
  path <- c("bucket=b", "region=r", "key=key_name", "stage=stage_name")
  expect_equal(store_gcp_key(path), "key_name")
})

tar_test("store_gcp_version()", {
  path <- c("bucket=b", "version=number")
  expect_equal(store_gcp_version(path), "number")
  expect_null(store_gcp_version(letters))
})

tar_test("package detection", {
  skip_cran()
  target <- tar_target(x, "x_value", format = "feather", repository = "gcp")
  out <- sort(store_get_packages(target$store))
  exp <- sort(c("googleCloudStorageR", "arrow"))
  expect_equal(out, exp)
})

tar_test("inherits from tar_external", {
  store <- tar_target(
    x,
    "x_value",
    format = "feather",
    repository = "gcp"
  )$store
  expect_true(inherits(store, "tar_external"))
})

tar_test("store_row_path()", {
  target <- tar_target(
    x,
    "x_value",
    format = "feather",
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
    format = "feather",
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

tar_test("validate gcp_feather", {
  skip_cran()
  skip_if_not_installed("googleCloudStorageR")
  skip_if_not_installed("arrow")
  target <- tar_target(x, "x_value", format = "feather", repository = "gcp")
  tar_script(list(target))
  expect_silent(tar_validate(callr_function = NULL))
})

tar_test("store_produce_path()", {
  store <- tar_target(
    x,
    "x_value",
    format = "rds",
    repository = "gcp"
  )$store
  store$resources <- list(bucket = "x_bucket")
  out <- store_produce_path(store, "x_name", "x_object")
  expect_equal(
    sort(out),
    sort(c("bucket=x_bucket", "key=_targets/objects/x_name"))
  )
})
