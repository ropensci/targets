tar_test("tar_resources_future()", {
  out <- tar_resources_future()
  expect_silent(resources_validate(out))
})
