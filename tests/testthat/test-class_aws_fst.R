tar_test("aws_fst packages", {
  target <- tar_target(x, "x_value", format = "aws_fst")
  out <- sort(store_get_packages(target$store))
  exp <- sort(c("paws", "fst"))
  expect_equal(out, exp)
})

tar_test("inherits from tar_external", {
  store <- tar_target(x, "x_value", format = "aws_fst")$store
  expect_true(inherits(store, "tar_external"))
})

tar_test("store_row_path()", {
  store <- tar_target(x, "x_value", format = "aws_fst")$store
  store$file$path <- "path"
  expect_equal(store_row_path(store), "path")
})

tar_test("store_path_from_record()", {
  store <- tar_target(x, "x_value", format = "aws_fst")$store
  record <- record_init(path = "path", format = "aws_fst")
  expect_equal(store_path_from_record(store, record), "path")
})

tar_test("validate aws_fst", {
  skip_if_not_installed("paws")
  skip_if_not_installed("fst")
  tar_script(list(tar_target(x, "x_value", format = "aws_fst")))
  expect_silent(tar_validate(callr_function = NULL))
})
