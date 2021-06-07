tar_test("deprecated unstructured resource list", {
  expect_warning(resources_validate(1), class = "tar_condition_deprecate")
})
