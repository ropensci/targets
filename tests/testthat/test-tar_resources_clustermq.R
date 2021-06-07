tar_test("tar_resources_clustermq()", {
  out <- tar_resources_clustermq()
  expect_silent(resources_validate(out))
})
