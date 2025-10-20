tar_test("tar_option_unset()", {
  on.exit(tar_option_reset())
  original_error <- tar_option_get("error")
  original_format <- tar_option_get("format")
  new_error <- "continue"
  new_format <- "file"
  expect_false(original_error == new_error)
  expect_false(original_format == new_format)
  tar_option_set(error = new_error, format = new_format)
  expect_equal(tar_option_get("error"), new_error)
  expect_equal(tar_option_get("format"), new_format)
  tar_option_unset(c("error", "format"))
  expect_equal(tar_option_get("error"), original_error)
  expect_equal(tar_option_get("format"), original_format)
})
