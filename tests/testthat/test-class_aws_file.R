tar_test("trust_object_timestamps = TRUE", {
  skip_cran()
  skip_on_os("windows")
  skip_if_not_installed("paws.storage")
  old <- tar_option_get("trust_object_timestamps")
  on.exit(tar_option_set(trust_object_timestamps = old))
  tar_option_set(trust_object_timestamps = TRUE)
  x <- target_init(
    name = "abc",
    expr = quote(a),
    repository = "aws",
    format = "file"
  )
  expect_false(x$store$file$trust_timestamps)
})

tar_test("trust_object_timestamps = FALSE", {
  skip_cran()
  skip_on_os("windows")
  skip_if_not_installed("paws.storage")
  old <- tar_option_get("trust_object_timestamps")
  on.exit(tar_option_set(trust_object_timestamps = old))
  tar_option_set(trust_object_timestamps = FALSE)
  x <- target_init(
    name = "abc",
    expr = quote(a),
    repository = "aws",
    format = "file"
  )
  expect_false(x$store$file$trust_timestamps)
})

tar_test("aws_file packages", {
  skip_cran()
  skip_on_os("windows")
  target <- tar_target(
    x,
    "x_value",
    format = "file",
    repository = "aws"
  )
  expect_equal(store_get_packages(target$store), "paws.storage")
})

tar_test("inherits from tar_external", {
  skip_cran()
  skip_on_os("windows")
  store <- tar_target(
    x,
    "x_value",
    format = "file",
    repository = "aws"
  )$store
  expect_true(inherits(store, "tar_external"))
})

tar_test("store_row_path()", {
  skip_cran()
  skip_on_os("windows")
  store <- tar_target(
    x,
    "x_value",
    format = "file",
    repository = "aws"
  )$store
  store$file$path <- "path"
  expect_equal(store_row_path(store), "path")
})

tar_test("store_path_from_record()", {
  skip_cran()
  skip_on_os("windows")
  store <- tar_target(
    x,
    "x_value",
    format = "file",
    repository = "aws"
  )$store
  record <- record_init(
    path = "path",
    format = "file",
    repository = "aws"
  )
  expect_equal(store_path_from_record(store, record), "path")
})

tar_test("store_aws_file_stage() with targets <= 0.4.2", {
  skip_cran()
  skip_on_os("windows")
  path <- c("bucket_name", "key_name")
  out <- store_aws_file_stage(path)
  expect_equal(dirname(out), file.path(path_scratch_dir_cloud(), "scratch"))
  expect_true(grepl("targets_aws_file_key_name", basename(out)))
})

tar_test("store_aws_file_stage() with targets <= 0.8.1", {
  path <- c("bucket_name", "key_name", "stage_name")
  expect_equal(store_aws_file_stage(path), "stage_name")
})

tar_test("store_aws_file_stage() with targets > 0.8.1", {
  path <- c("bucket=b", "region=r", "key=key_name", "stage=stage_name")
  expect_equal(store_aws_file_stage(path), "stage_name")
})

tar_test("validate aws file", {
  skip_cran()
  skip_on_os("windows")
  skip_if_not_installed("paws.storage")
  tar_script(
    list(
      tar_target(
        x,
        "x_value",
        format = "file",
        repository = "aws"
      )
    )
  )
  expect_silent(tar_validate(callr_function = NULL))
})
