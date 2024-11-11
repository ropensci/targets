tar_test("create tar_resources_qs object", {
  x <- resources_qs_init()
  expect_silent(resources_validate(x))
})

tar_test("prohibit empty tar_resources_qs object", {
  expect_error(
    resources_qs_init(compress_level = NULL),
    class = "tar_condition_validate"
  )
})

tar_test("print tar_resources_qs object", {
  x <- resources_qs_init()
  out <- utils::capture.output(print(x))
  expect_true(any(grepl("tar_resources_qs", out)))
})
