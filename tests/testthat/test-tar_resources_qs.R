tar_test("tar_resources_qs()", {
  out <- tar_resources_qs()
  expect_silent(resources_validate(out))
})
