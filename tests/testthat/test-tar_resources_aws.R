tar_test("tar_resources_aws()", {
  out <- tar_resources_aws(bucket = "bucket123")
  expect_equal(out$bucket, "bucket123")
  expect_null(out$region)
  expect_silent(resources_validate(out))
})

tar_test("tar_resources_aws() with region", {
  out <- tar_resources_aws(bucket = "bucket123", region = "us-east-1")
  expect_equal(out$region, "us-east-1")
  expect_silent(resources_validate(out))
})

tar_test("tar_resources_aws() with part_size", {
  out <- tar_resources_aws(bucket = "bucket123", part_size = 1e8)
  expect_equal(out$part_size, 1e8)
  expect_silent(resources_validate(out))
})
