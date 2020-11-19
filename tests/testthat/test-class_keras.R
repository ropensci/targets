tar_test("validate keras format", {
  skip_if_not_installed("keras")
  x <- target_init(name = "abc", expr = quote(f()), format = "keras")
  expect_silent(target_validate(x))
})

tar_test("keras packages", {
  x <- tar_target(x, 1, format = "keras")
  out <- store_get_packages(x$store)
  expect_equal(out, "keras")
})
