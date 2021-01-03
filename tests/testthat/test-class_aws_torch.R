tar_test("aws_torch packages", {
  target <- tar_target(x, "x_value", format = "aws_torch")
  out <- sort(store_get_packages(target$store))
  exp <- sort(c("aws.s3", "torch"))
  expect_equal(out, exp)
})

tar_test("validate aws_torch", {
  skip_if_not_installed("aws.s3")
  skip_if_not_installed("torch")
  tar_script(list(tar_target(x, "x_value", format = "aws_torch")))
  expect_silent(tar_validate(callr_function = NULL))
})
