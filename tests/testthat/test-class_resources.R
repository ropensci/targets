tar_test("create tar_resources object", {
  x <- resources_init()
  expect_silent(resources_validate(x))
})

tar_test("invalid resource component", {
  x <- resources_init(aws = 123)
  expect_error(resources_validate(x), class = "tar_condition_validate")
})

tar_test("print tar_resources object", {
  x <- resources_init()
  out <- utils::capture.output(print(x))
  expect_true(any(grepl("tar_resources_aws", out)))
})
