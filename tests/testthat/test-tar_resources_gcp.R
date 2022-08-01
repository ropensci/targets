tar_test("tar_resources_gcp()", {
  out <- tar_resources_gcp(bucket = "bucket123")
  expect_equal(out$bucket, "bucket123")
  expect_null(out$region)
  expect_silent(resources_validate(out))
})

tar_test("tar_resources_gcp() with prefix", {
  out <- tar_resources_gcp(bucket = "bucket123", prefix = "us-east-1")
  expect_equal(out$prefix, "us-east-1")
  expect_silent(resources_validate(out))
})

tar_test("tar_resources_gcp() default bucket", {
  tar_option_set(
    resources = tar_resources(
      gcp = tar_resources_gcp(
        bucket = "non_default"
      )
    )
  )
  out <- tar_resources_gcp()
  expect_equal(out$bucket, "non_default")
})

tar_test("tar_resources_gcp() default prefix", {
  tar_option_set(
    resources = tar_resources(
      gcp = tar_resources_gcp(
        prefix = "non_default"
      )
    )
  )
  out <- tar_resources_gcp()
  expect_equal(out$prefix, "non_default")
})

tar_test("tar_resources_gcp() default predefined_acl", {
  tar_option_set(
    resources = tar_resources(
      gcp = tar_resources_gcp(
        predefined_acl = "non_default"
      )
    )
  )
  out <- tar_resources_gcp()
  expect_equal(out$predefined_acl, "non_default")
})

tar_test("tar_resources_gcp() default predefined_acl", {
  tar_option_set(
    resources = tar_resources(
      gcp = tar_resources_gcp(
        verbose = TRUE
      )
    )
  )
  out <- tar_resources_gcp()
  expect_equal(out$verbose, TRUE)
})
