tar_test("tar_format_get()", {
  skip_cran()
  skip_if_not_installed("qs2")
  expect_equal(tar_format_get(), tar_option_get("format"))
  tar_script(tar_target(x, tar_format_get(), format = "qs"))
  tar_make(callr_function = NULL)
  expect_equal(tar_read(x), "qs")
})
