tar_test("tar_resources_aws()", {
  out <- tar_resources_aws(bucket = "123")
  expect_silent(resources_validate(out))
})
