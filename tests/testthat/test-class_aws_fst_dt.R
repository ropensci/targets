tar_test("aws_fst_dt packages", {
  target <- tar_target(x, "x_value", format = "aws_fst_dt")
  out <- sort(store_get_packages(target$store))
  exp <- sort(c("aws.s3", "fst", "data.table"))
  expect_equal(out, exp)
})

tar_test("validate aws_fst_dt", {
  skip_if_not_installed("aws.s3")
  skip_if_not_installed("fst")
  skip_if_not_installed("data.table")
  tar_script(list(tar_target(x, "x_value", format = "aws_fst_dt")))
  expect_silent(tar_validate(callr_function = NULL))
})
