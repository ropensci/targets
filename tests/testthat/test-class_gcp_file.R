tar_test("trust_object_timestamps = TRUE", {
  skip_if_not_installed("googleCloudStorageR")
  old <- tar_option_get("trust_object_timestamps")
  on.exit(tar_option_set(trust_object_timestamps = old))
  tar_option_set(trust_object_timestamps = TRUE)
  x <- target_init(
    name = "abc",
    expr = quote(a),
    repository = "gcp",
    format = "file"
  )
  expect_false(x$store$file$trust_timestamps)
})

tar_test("trust_object_timestamps = FALSE", {
  skip_if_not_installed("googleCloudStorageR")
  old <- tar_option_get("trust_object_timestamps")
  on.exit(tar_option_set(trust_object_timestamps = old))
  tar_option_set(trust_object_timestamps = FALSE)
  x <- target_init(
    name = "abc",
    expr = quote(a),
    repository = "gcp",
    format = "file"
  )
  expect_false(x$store$file$trust_timestamps)
})

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
  store <- tar_target(
    x,
    "x_value",
    format = "file",
    repository = "gcp"
  )$store
  store$file$path <- "path"
  expect_equal(store_row_path(store), "path")
})

tar_test("store_path_from_record()", {
  store <- tar_target(
    x,
    "x_value",
    format = "file",
    repository = "gcp"
  )$store
  record <- record_init(
    path = "path",
    format = "file",
    repository = "gcp"
  )
  expect_equal(store_path_from_record(store, record), "path")
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
