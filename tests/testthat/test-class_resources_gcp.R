tar_test("create tar_resources_gcp object", {
  x <- resources_gcp_init(bucket = "bucket_name")
  expect_silent(resources_validate(x))
})

tar_test("prohibit empty tar_resources_gcp object", {
  x <- resources_gcp_init(bucket = "", prefix = NULL)
  expect_error(resources_validate(x), class = "tar_condition_validate")
})

tar_test("print tar_resources_gcp object", {
  x <- resources_gcp_init(bucket = "bucket_name")
  out <- utils::capture.output(print(x))
  expect_true(any(grepl("tar_resources_gcp", out)))
})
