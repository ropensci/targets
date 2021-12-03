tar_test("aws_keras packages", {
  target <- tar_target(x, "x_value", format = "aws_keras")
  out <- sort(store_get_packages(target$store))
  exp <- sort(c("paws", "keras"))
  expect_equal(out, exp)
})

tar_test("inherits from tar_external", {
  store <- tar_target(x, "x_value", format = "aws_keras")$store
  expect_true(inherits(store, "tar_external"))
})

tar_test("store_row_path()", {
  store <- tar_target(x, "x_value", format = "aws_keras")$store
  store$file$path <- "path"
  expect_equal(store_row_path(store), "path")
})

tar_test("store_path_from_record()", {
  store <- tar_target(x, "x_value", format = "aws_keras")$store
  record <- record_init(path = "path", format = "aws_keras")
  expect_equal(store_path_from_record(store, record), "path")
})

tar_test("validate aws_keras", {
  skip_if_not_installed("paws")
  skip_if_not_installed("keras")
  tar_script(list(tar_target(x, "x_value", format = "aws_keras")))
  expect_silent(tar_validate(callr_function = NULL))
})
