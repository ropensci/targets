tar_test("create tar_resources_qs object", {
  x <- resources_qs_init(preset = "high")
  expect_silent(resources_validate(x))
})

tar_test("prohibit empty tar_resources_qs object", {
  x <- resources_qs_init(preset = NULL)
  expect_error(resources_validate(x), class = "tar_condition_validate")
})

tar_test("print tar_resources_qs object", {
  x <- resources_qs_init(preset = "high")
  out <- utils::capture.output(print(x))
  expect_true(any(grepl("tar_resources_qs", out)))
})
