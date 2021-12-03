tar_test("aws_fst_dt packages", {
  target <- tar_target(x, "x_value", format = "aws_fst_dt")
  out <- sort(store_get_packages(target$store))
  exp <- sort(c("paws", "fst", "data.table"))
  expect_equal(out, exp)
})

tar_test("inherits from tar_external", {
  store <- tar_target(x, "x_value", format = "aws_fst_dt")$store
  expect_true(inherits(store, "tar_external"))
})

tar_test("store_row_path()", {
  store <- tar_target(x, "x_value", format = "aws_fst_dt")$store
  store$file$path <- "path"
  expect_equal(store_row_path(store), "path")
})

tar_test("store_path_from_record()", {
  store <- tar_target(x, "x_value", format = "aws_fst_dt")$store
  record <- record_init(path = "path", format = "aws_fst_dt")
  expect_equal(store_path_from_record(store, record), "path")
})

tar_test("validate aws_fst_dt", {
  skip_if_not_installed("paws")
  skip_if_not_installed("fst")
  skip_if_not_installed("data.table")
  tar_script(list(tar_target(x, "x_value", format = "aws_fst_dt")))
  expect_silent(tar_validate(callr_function = NULL))
})
