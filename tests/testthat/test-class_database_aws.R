tar_test("validate AWS database", {
  skip_if_not_installed("paws.storage")
  out <- database_init(
    repository = "aws",
    resources = tar_resources(
      aws = tar_resources_aws(bucket = "x", prefix = "x")
    )
  )
  expect_silent(out$validate())
})
