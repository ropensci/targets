tar_test("create tar_resources_clustermq object", {
  x <- resources_clustermq_init(template = list())
  expect_silent(resources_validate(x))
})

tar_test("duplicated template", {
  x <- resources_clustermq_init(template = list(a = 1, a = 2))
  expect_error(resources_validate(x), class = "tar_condition_validate")
})

tar_test("print tar_resources_clustermq object", {
  x <- resources_clustermq_init(template = list(a = 1))
  out <- utils::capture.output(print(x))
  expect_true(any(grepl("tar_resources_clustermq", out)))
})
