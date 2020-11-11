tar_test("tar_target() works", {
  x <- tar_target(x, get_data())
  expect_silent(target_validate(x))
  expect_equal(target_get_name(x), "x")
  expect_equal(x$command$string, "expression(get_data())")
})

tar_test("tar_target() gets priorities", {
  x <- tar_target(x, get_data(), priority = 0.5)
  expect_equal(x$settings$priority, 0.5)
})

tar_test("tar_target() defines pattens correctly", {
  x <- tar_target(x, 1, pattern = map(y))
  expect_silent(target_validate(x))
  expect_equal(x$settings$pattern, expression(map(y)))
  expect_equal(x$settings$dimensions, "y")
})

tar_test("tar_target() receives options", {
  tar_option_set(format = "file")
  x <- tar_target(x, "y")
  expect_equal(x$settings$format, "file")
})

tar_test("tidy eval works", {
  envir <- environment()
  tar_option_set(envir = envir)
  envir$y <- 1
  x <- tar_target(x, get_data(!!y))
  expect_equal(x$command$string, "expression(get_data(1))")
})

tar_test("can disable tidy eval", {
  y <- 1
  x <- tar_target(x, get_data(!!y), tidy_eval = FALSE)
  expect_equal(x$command$string, "expression(get_data(!!y))")
})
