tar_test("aws_qs packages", {
  target <- tar_target(x, "x_value", format = "aws_qs")
  out <- sort(store_get_packages(target$store))
  exp <- sort(c("aws.s3", "qs"))
  expect_equal(out, exp)
})

tar_test("inherits from tar_external", {
  store <- tar_target(x, "x_value", format = "aws_qs")$store
  expect_true(inherits(store, "tar_external"))
})

tar_test("store_row_path()", {
  store <- tar_target(x, "x_value", format = "aws_qs")$store
  store$file$path <- "path"
  expect_equal(store_row_path(store), "path")
})

tar_test("store_path_from_record()", {
  store <- tar_target(x, "x_value", format = "aws_qs")$store
  record <- record_init(path = "path", format = "aws_qs")
  expect_equal(store_path_from_record(store, record), "path")
})

tar_test("validate aws_qs", {
  skip_if_not_installed("aws.s3")
  skip_if_not_installed("qs")
  tar_script(list(tar_target(x, "x_value", format = "aws_qs")))
  expect_silent(tar_validate(callr_function = NULL))
})
