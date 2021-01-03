tar_test("aws_fst packages", {
  target <- tar_target(x, "x_value", format = "aws_fst")
  out <- sort(store_get_packages(target$store))
  exp <- sort(c("aws.s3", "fst"))
  expect_equal(out, exp)
})

tar_test("validate aws_fst", {
  skip_if_not_installed("aws.s3")
  skip_if_not_installed("fst")
  tar_script(list(tar_target(x, "x_value", format = "aws_fst")))
  expect_silent(tar_validate(callr_function = NULL))
})
