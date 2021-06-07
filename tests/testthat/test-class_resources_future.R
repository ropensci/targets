tar_test("create tar_resources_future object", {
  x <- resources_future_init(resources = list())
  expect_silent(resources_validate(x))
})

tar_test("duplicated resources", {
  x <- resources_future_init(resources = list(a = 1, a = 2))
  expect_error(resources_validate(x), class = "tar_condition_validate")
})

tar_test("print tar_resources_future object", {
  x <- resources_future_init(resources = list(a = 1))
  out <- utils::capture.output(print(x))
  expect_true(any(grepl("tar_resources_future", out)))
})
