tar_test("validate GCP database", {
  out <- database_init(
    repository = "gcp",
    resources = tar_resources(gcp = tar_resources_gcp(prefix = "x"))
  )
  expect_silent(out$validate())
})
