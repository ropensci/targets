tar_test("tar_resources_fst()", {
  out <- tar_resources_fst()
  expect_silent(resources_validate(out))
})
