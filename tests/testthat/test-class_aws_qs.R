tar_test("aws_qs packages", {
  target <- tar_target(x, "x_value", format = "aws_qs")
  out <- sort(store_get_packages(target$store))
  exp <- sort(c("aws.s3", "qs"))
  expect_equal(out, exp)
})

tar_test("validate aws_qs", {
  skip_if_not_installed("aws.s3")
  skip_if_not_installed("qs")
  tar_script(list(tar_target(x, "x_value", format = "aws_qs")))
  expect_silent(tar_validate(callr_function = NULL))
})
