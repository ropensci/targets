tar_test("tar_resources_feather()", {
  out <- tar_resources_feather()
  expect_silent(resources_validate(out))
})
