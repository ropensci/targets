tar_test("aws_keras packages", {
  target <- tar_target(x, "x_value", format = "aws_keras")
  out <- sort(store_get_packages(target$store))
  exp <- sort(c("aws.s3", "keras"))
  expect_equal(out, exp)
})

tar_test("validate aws_keras", {
  skip_if_not_installed("aws.s3")
  skip_if_not_installed("keras")
  tar_script(list(tar_target(x, "x_value", format = "aws_keras")))
  expect_silent(tar_validate(callr_function = NULL))
})
