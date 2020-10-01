tar_test("validate torch format", {
  skip_if_not_installed("torch")
  x <- target_init(name = "a", expr = quote(f()), format = "torch")
  expect_silent(target_validate(x))
})
