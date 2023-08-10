tar_test("create tar_resources_aws object", {
  skip_cran()
  skip_on_os("windows")
  skip_if_not_installed("paws.storage")
  x <- resources_aws_init(bucket = "bucket_name")
  expect_silent(resources_validate(x))
})

tar_test("prohibit empty tar_resources_aws object", {
  skip_cran()
  skip_on_os("windows")
  skip_if_not_installed("paws.storage")
  x <- resources_aws_init(bucket = "", prefix = NULL)
  expect_error(resources_validate(x), class = "tar_condition_validate")
})

tar_test("print tar_resources_aws object", {
  skip_cran()
  skip_on_os("windows")
  skip_if_not_installed("paws.storage")
  x <- resources_aws_init(bucket = "bucket_name")
  out <- utils::capture.output(print(x))
  expect_true(any(grepl("tar_resources_aws", out)))
})
