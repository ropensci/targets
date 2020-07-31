tar_test("tar_option_set() works", {
  tar_option_set(packages = "tidyverse")
  expect_equal(tar_option("packages"), "tidyverse")
})

tar_test("tar_option() must take a valid option name", {
  expect_error(tar_option("nope"))
})
