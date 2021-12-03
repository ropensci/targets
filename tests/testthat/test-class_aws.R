tar_test("bucket with tech debt: 0.8.1.9000 metabucket format", {
  expect_equal(store_aws_bucket("bucket=abc:region=xyz"), "abc")
})

tar_test("region with tech debt: 0.8.1.9000 metabucket format", {
  expect_equal(store_aws_bucket("bucket=abc:region=xyz"), "abc")
})

tar_test("NULL region", {
  path <- c("bucket=abc", "region=NULL", "key=stuff")
  expect_null(store_aws_region(path))
})

tar_test("\"\" region", {
  path <- c("bucket=abc", "region=", "key=stuff")
  expect_equal(store_aws_region(path), "")
})

tar_test("compat with targets <= 0.8.1", {
  path <- c("bucket_name", "object_name", "stage_name")
  expect_equal(store_aws_bucket(path), "bucket_name")
  expect_null(store_aws_region(path))
  expect_equal(store_aws_key(path), "object_name")
})

tar_test("store_aws_key() with targets <= 0.8.1", {
  path <- c("bucket_name", "key_name", "stage_name")
  expect_equal(store_aws_key(path), "key_name")
})

tar_test("store_aws_key() with targets > 0.8.1", {
  path <- c("bucket=b", "region=r", "key=key_name", "stage=stage_name")
  expect_equal(store_aws_key(path), "key_name")
})

tar_test("store_aws_version()", {
  path <- c("bucket=b", "version=number")
  expect_equal(store_aws_version(path), "number")
  expect_null(store_aws_version(letters))
})

tar_test("store_aws_split_colon()", {
  path <- c("bucket=bu:region=reg", "key=sdfasdf")
  expect_equal(
    store_aws_split_colon(path),
    c("bucket=bu", "region=reg", "key=sdfasdf")
  )
  path <- c("key=sdfasdf", "bucket=bu:region=reg")
  expect_equal(
    store_aws_split_colon(path),
    c("bucket=bu", "region=reg", "key=sdfasdf")
  )
})

tar_test("store_aws_field()", {
  path <- c("bucket=bu", "region=reg", "key=sdfasdf")
  expect_equal(
    store_aws_field(path = path, pattern = "^bucket="),
    "bu"
  )
  expect_equal(
    store_aws_field(path = path, pattern = "^region="),
    "reg"
  )
  expect_equal(
    store_aws_field(path = path, pattern = "^key="),
    "sdfasdf"
  )
})
