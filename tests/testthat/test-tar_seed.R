tar_test("deprecate tar_seed()", {
  expect_warning(tar_seed(), class = "tar_condition_deprecate")
})
