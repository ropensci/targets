tar_test("tar_resources_url()", {
  out <- tar_resources_url()
  expect_silent(resources_validate(out))
})
