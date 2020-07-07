tar_test("tar_target_external() works", {
  envir <- new.env(parent = baseenv())
  x <- tar_target_external("x", expression(get_data()), envir = envir)
  expect_silent(target_validate(x))
  expect_equal(target_get_name(x), "x")
  expect_equal(x$command$string, "expression(get_data())")
})

tar_test("tar_target_external() defines pattens correctly", {
  x <- tar_target_external("x", expression(1), pattern = expression(map(y)))
  expect_silent(target_validate(x))
  expect_equal(x$settings$growth, "map")
  expect_equal(x$settings$dimensions, "y")
})

tar_test("tar_target_external() receives options", {
  on.exit(tar_options())
  tar_options(format = "file")
  x <- tar_target_external("x", "y")
  expect_equal(x$settings$format, "file")
})
