tar_test("tar_target_raw() works", {
  envir <- new.env(parent = baseenv())
  x <- tar_target_raw("x", expression(get_data()), envir = envir)
  expect_silent(target_validate(x))
  expect_equal(target_get_name(x), "x")
  expect_equal(x$command$string, "expression(get_data())")
})

tar_test("tar_target_raw() gets priorities", {
  x <- tar_target_raw("x", quote(get_data()), priority = 0.5)
  expect_equal(x$settings$priority, 0.5)
})

tar_test("tar_target_raw() defines pattens correctly", {
  x <- tar_target_raw("x", expression(1), pattern = expression(map(y)))
  expect_silent(target_validate(x))
  expect_equal(x$settings$growth, "map")
  expect_equal(x$settings$dimensions, "y")
})

tar_test("tar_target_raw() receives options", {
  on.exit(tar_option_set())
  tar_option_set(format = "file")
  x <- tar_target_raw("x", "y")
  expect_equal(x$settings$format, "file")
})
