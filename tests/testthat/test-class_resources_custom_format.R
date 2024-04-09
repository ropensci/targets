tar_test("create tar_resources_custom_format object", {
  x <- resources_custom_format_init(envvars = c(x = "x"))
  expect_silent(resources_validate(x))
})

tar_test("print tar_resources_custom_format object", {
  x <- resources_custom_format_init(envvars = c(x = "x"))
  out <- utils::capture.output(print(x))
  expect_true(any(grepl("tar_resources_custom_format", out)))
})
