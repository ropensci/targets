tar_test("tar_target() works", {
  x <- tar_target(x, get_data())
  expect_silent(target_validate(x))
  expect_equal(target_get_name(x), "x")
  expect_equal(x$command$string, "expression(get_data())")
  expect_equal(x$settings$description, character(0L))
})

tar_test("tar_target() description", {
  x <- tar_target(x, get_data(), description = "info")
  expect_silent(target_validate(x))
  expect_equal(x$settings$description, "info")
})

tar_test("tar_target() gets priorities", {
  x <- tar_target(x, get_data(), priority = 0)
  expect_equal(x$settings$priority, 0)
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

tar_test("no name", {
  expect_error(tar_target(command = 1), class = "tar_condition_validate")
})

tar_test("no command", {
  expect_error(tar_target(abc), class = "tar_condition_validate")
})

tar_test("declaring a target does not run its command", {
  x <- tar_target(y, file.create("x"))
  expect_false(file.exists("x"))
})

tar_test("superseded ultra-custom formats and error null", {
  for (format in c("keras", "torch")) {
    expect_warning(
      tmp <- tar_target(x, "y", format = format, error = "null"),
      class = "tar_condition_deprecate"
    )
  }
  for (format in c("rds", "qs")) {
    expect_silent(
      tmp <- tar_target(x, "y", format = format, error = "null")
    )
  }
})

tar_test("url format has local repository", {
  target <- tar_target(x, 1, repository = "aws", format = "url")
  expect_equal(target$settings$repository,  "local")
})
