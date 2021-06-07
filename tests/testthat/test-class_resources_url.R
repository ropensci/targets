tar_test("create tar_resources_url object", {
  skip_if_not_installed("curl")
  x <- resources_url_init(handle = curl::new_handle())
  expect_silent(resources_validate(x))
})

tar_test("prohibit empty tar_resources_url object", {
  x <- resources_url_init(handle = character(0))
  expect_error(resources_validate(x), class = "tar_condition_validate")
})

tar_test("print tar_resources_url object", {
  skip_if_not_installed("curl")
  x <- resources_url_init(handle = curl::new_handle())
  out <- utils::capture.output(print(x))
  expect_true(any(grepl("tar_resources_url", out)))
})
