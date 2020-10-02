tar_test("validate aws_rds", {
  skip_if_not_installed("aws.s3")
  tar_script(tar_pipeline(tar_target(x, "x_value", format = "aws_rds")))
  expect_silent(tar_validate(callr_function = NULL))
})

tar_test("validate aws_qs", {
  skip_if_not_installed("aws.s3")
  skip_if_not_installed("qs")
  tar_script(tar_pipeline(tar_target(x, "x_value", format = "aws_qs")))
  expect_silent(tar_validate(callr_function = NULL))
})

tar_test("validate aws_file", {
  skip_if_not_installed("aws.s3")
  tar_script(tar_pipeline(tar_target(x, "x_value", format = "aws_file")))
  expect_silent(tar_validate(callr_function = NULL))
})

tar_test("validate aws_fst", {
  skip_if_not_installed("aws.s3")
  skip_if_not_installed("fst")
  tar_script(tar_pipeline(tar_target(x, "x_value", format = "aws_fst")))
  expect_silent(tar_validate(callr_function = NULL))
})

tar_test("validate aws_fst_dt", {
  skip_if_not_installed("aws.s3")
  skip_if_not_installed("fst")
  skip_if_not_installed("data.table")
  tar_script(tar_pipeline(tar_target(x, "x_value", format = "aws_fst_dt")))
  expect_silent(tar_validate(callr_function = NULL))
})

tar_test("validate aws_fst_tbl", {
  skip_if_not_installed("aws.s3")
  skip_if_not_installed("fst")
  skip_if_not_installed("tibble")
  tar_script(tar_pipeline(tar_target(x, "x_value", format = "aws_fst_tbl")))
  expect_silent(tar_validate(callr_function = NULL))
})

tar_test("validate aws_keras", {
  skip_if_not_installed("aws.s3")
  skip_if_not_installed("keras")
  tar_script(tar_pipeline(tar_target(x, "x_value", format = "aws_keras")))
  expect_silent(tar_validate(callr_function = NULL))
})

tar_test("validate aws_torch", {
  skip_if_not_installed("aws.s3")
  skip_if_not_installed("torch")
  tar_script(tar_pipeline(tar_target(x, "x_value", format = "aws_torch")))
  expect_silent(tar_validate(callr_function = NULL))
})
