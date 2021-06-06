tar_test("create tar_resources_future object", {
  x <- resources_future_init(log = "dir")
  expect_silent(resources_validate(x))
})

tar_test("print tar_resources_future object", {
  x <- resources_future_init(log = "dir")
  out <- utils::capture.output(print(x))
  expect_true(any(grepl("tar_resources_future", out)))
})
