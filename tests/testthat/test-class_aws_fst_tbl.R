tar_test("aws_fst_tbl packages", {
  target <- tar_target(x, "x_value", format = "aws_fst_tbl")
  out <- sort(store_get_packages(target$store))
  exp <- sort(c("aws.s3", "fst", "tibble"))
  expect_equal(out, exp)
})

tar_test("inherits from tar_external", {
  store <- tar_target(x, "x_value", format = "aws_fst_tbl")$store
  expect_true(inherits(store, "tar_external"))
})

tar_test("store_row_path()", {
  store <- tar_target(x, "x_value", format = "aws_fst_tbl")$store
  store$file$path <- "path"
  expect_equal(store_row_path(store), "path")
})

tar_test("store_path_from_record()", {
  store <- tar_target(x, "x_value", format = "aws_fst_tbl")$store
  record <- record_init(path = "path", format = "aws_fst_tbl")
  expect_equal(store_path_from_record(store, record), "path")
})

tar_test("validate aws_fst_tbl", {
  skip_if_not_installed("aws.s3")
  skip_if_not_installed("fst")
  skip_if_not_installed("tibble")
  tar_script(list(tar_target(x, "x_value", format = "aws_fst_tbl")))
  expect_silent(tar_validate(callr_function = NULL))
})
