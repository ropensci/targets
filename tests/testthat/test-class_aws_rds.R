tar_test("aws_rds packages", {
  target <- tar_target(x, "x_value", format = "aws_rds")
  expect_equal(store_get_packages(target$store), "aws.s3")
})

tar_test("inherits from tar_external", {
  store <- tar_target(x, "x_value", format = "aws_rds")$store
  expect_true(inherits(store, "tar_external"))
})

tar_test("store_row_path()", {
  store <- tar_target(x, "x_value", format = "aws_rds")$store
  store$file$path <- "path"
  expect_equal(store_row_path(store), "path")
})

tar_test("store_path_from_record()", {
  store <- tar_target(x, "x_value", format = "aws_rds")$store
  record <- record_init(path = "path", format = "aws_rds")
  expect_equal(store_path_from_record(store, record), "path")
})

tar_test("validate aws_rds", {
  skip_if_not_installed("aws.s3")
  tar_script(list(tar_target(x, "x_value", format = "aws_rds")))
  expect_silent(tar_validate(callr_function = NULL))
})
