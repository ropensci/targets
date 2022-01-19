tar_test("tar_resources_gcp()", {
  out <- tar_resources_gcp(bucket = "bucket123")
  expect_equal(out$bucket, "bucket123")
  expect_null(out$region)
  expect_silent(resources_validate(out))
})
