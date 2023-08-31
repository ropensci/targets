tar_test("validate GCP database", {
  skip_if_not_installed("googleCloudStorageR")
  out <- database_init(
    repository = "gcp",
    resources = tar_resources(
      gcp = tar_resources_gcp(bucket = "x", prefix = "x")
    )
  )
  expect_silent(out$validate())
})
