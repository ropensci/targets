tar_test("aws_feather packages", {
  skip_on_cran()
  target <- tar_target(x, "x_value", format = "aws_feather")
  out <- sort(store_get_packages(target$store))
  exp <- sort(c("aws.s3", "arrow"))
  expect_equal(out, exp)
})

tar_test("inherits from tar_external", {
  store <- tar_target(x, "x_value", format = "aws_feather")$store
  expect_true(inherits(store, "tar_external"))
})

tar_test("store_row_path()", {
  store <- tar_target(x, "x_value", format = "aws_feather")$store
  store$file$path <- "path"
  expect_equal(store_row_path(store), "path")
})

tar_test("store_path_from_record()", {
  store <- tar_target(x, "x_value", format = "aws_feather")$store
  record <- record_init(path = "path", format = "aws_feather")
  expect_equal(store_path_from_record(store, record), "path")
})

tar_test("validate aws_feather", {
  skip_on_cran()
  skip_if_not_installed("aws.s3")
  skip_if_not_installed("arrow")
  tar_script(list(tar_target(x, "x_value", format = "aws_feather")))
  expect_silent(tar_validate(callr_function = NULL))
})
