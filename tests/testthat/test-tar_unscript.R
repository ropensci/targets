tar_test("tar_unscript()", {
  dir.create(path_script_r())
  tar_unscript()
  expect_false(file.exists(path_script_r()))
})
