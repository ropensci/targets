tar_test("create tar_resources_fst object", {
  x <- resources_fst_init(compress = 50)
  expect_silent(resources_validate(x))
})

tar_test("prohibit empty tar_resources_fst object", {
  x <- resources_fst_init(compress = NULL)
  expect_error(resources_validate(x), class = "tar_condition_validate")
})

tar_test("print tar_resources_fst object", {
  x <- resources_fst_init(compress = 50)
  out <- utils::capture.output(print(x))
  expect_true(any(grepl("tar_resources_fst", out)))
})
