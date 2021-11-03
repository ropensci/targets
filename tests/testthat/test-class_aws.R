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
