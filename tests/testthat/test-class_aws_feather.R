tar_test("aws_feather packages", {
  skip_on_cran()
  target <- tar_target(x, "x_value", format = "aws_feather")
  out <- sort(store_get_packages(target$store))
  exp <- sort(c("aws.s3", "arrow"))
  expect_equal(out, exp)
})

tar_test("validate aws_feather", {
  skip_on_cran()
  skip_if_not_installed("aws.s3")
  skip_if_not_installed("arrow")
  tar_script(list(tar_target(x, "x_value", format = "aws_feather")))
  expect_silent(tar_validate(callr_function = NULL))
})
