tar_test("tar_target_raw() works", {
  tar_option_set(envir = new.env(parent = baseenv()))
  x <- tar_target_raw("x", expression(get_data()))
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
  expect_equal(x$settings$pattern, expression(map(y)))
  expect_equal(x$settings$dimensions, "y")
})

tar_test("tar_target_raw() receives options", {
  tar_option_set(format = "file")
  x <- tar_target_raw("x", "y")
  expect_equal(x$settings$format, "file")
})

tar_test("can set deps", {
  out <- tar_target_raw(
    "notebook",
    command = quote(abc)
  )
  expect_equal(out$command$deps, "abc")
  expect_true(grepl("abc", out$command$string))
  out <- tar_target_raw(
    "notebook",
    command = quote(abc),
    deps = "xyz"
  )
  expect_equal(out$command$deps, "xyz")
  expect_true(grepl("abc", out$command$string))
})

tar_test("can set string", {
  out <- tar_target_raw(
    "notebook",
    command = quote(abc)
  )
  expect_equal(out$command$deps, "abc")
  expect_true(grepl("abc", out$command$string))
  out <- tar_target_raw(
    "notebook",
    command = quote(abc),
    string = "xyz"
  )
  expect_equal(out$command$deps, "abc")
  expect_equal(out$command$string, "xyz")
})
