tar_test("tar_option_set() works", {
  tar_option_set(packages = "tidyverse")
  expect_equal(tar_option_get("packages"), "tidyverse")
})

tar_test("tar_option_get() must take a valid option name", {
  expect_error(tar_option_get("nope"))
})

tar_test("tar_option_get() must take a valid option name", {
  expect_error(tar_option_get("nope"))
})
