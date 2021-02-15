tar_test("tar_exist_script()", {
  expect_false(tar_exist_script())
  tar_script()
  expect_true(tar_exist_script())
})
