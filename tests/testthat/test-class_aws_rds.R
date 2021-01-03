tar_test("aws_rds packages", {
  target <- tar_target(x, "x_value", format = "aws_rds")
  expect_equal(store_get_packages(target$store), "aws.s3")
})

tar_test("validate aws_rds", {
  skip_if_not_installed("aws.s3")
  tar_script(list(tar_target(x, "x_value", format = "aws_rds")))
  expect_silent(tar_validate(callr_function = NULL))
})
