tar_test("metabucket with region", {
  metabucket <- store_produce_aws_metabucket(bucket = "abc", region = "xyz")
  expect_equal(metabucket, "bucket=abc:region=xyz")
  path <- c(metabucket, "object_name")
  expect_equal(store_aws_bucket(path), "abc")
  expect_equal(store_aws_region(path), "xyz")
})

tar_test("metabucket without region", {
  metabucket <- store_produce_aws_metabucket(bucket = "abc", region = NULL)
  expect_equal(metabucket, "bucket=abc:region=")
  path <- c(metabucket, "object_name")
  expect_equal(store_aws_bucket(path), "abc")
  expect_null(store_aws_region(path))
})

tar_test("metabucket compat with targets <= 0.8.1", {
  path <- c("bucket_name", "object_name")
  expect_equal(store_aws_bucket(path), "bucket_name")
  expect_null(store_aws_region(path))
})

tar_test("store_aws_key()", {
  path <- c("bucket_name", "key_name", "stage_name")
  expect_equal(store_aws_key(path), "key_name")
})

tar_test("store_aws_path()", {
  path <- c("bucket_name", "key_name", "stage_name")
  expect_equal(store_aws_path(path), "stage_name")
})
