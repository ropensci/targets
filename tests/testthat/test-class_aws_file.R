tar_test("aws_file packages", {
  target <- tar_target(x, "x_value", format = "aws_file")
  expect_equal(store_get_packages(target$store), "aws.s3")
})

tar_test("inherits from tar_external", {
  store <- tar_target(x, "x_value", format = "aws_file")$store
  expect_true(inherits(store, "tar_external"))
})

tar_test("store_row_path()", {
  store <- tar_target(x, "x_value", format = "aws_file")$store
  store$file$path <- "path"
  expect_equal(store_row_path(store), "path")
})

tar_test("store_path_from_record()", {
  store <- tar_target(x, "x_value", format = "aws_file")$store
  record <- record_init(path = "path", format = "aws_file")
  expect_equal(store_path_from_record(store, record), "path")
})

tar_test("validate aws_file", {
  skip_if_not_installed("aws.s3")
  tar_script(list(tar_target(x, "x_value", format = "aws_file")))
  expect_silent(tar_validate(callr_function = NULL))
})
