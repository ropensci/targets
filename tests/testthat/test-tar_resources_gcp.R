tar_test("tar_resources_gcp()", {
  out <- tar_resources_gcp(bucket = "bucket123", prefix = "x")
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
        bucket = "non_default",
        prefix = "x"
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
        prefix = "non_default",
        bucket = "x"
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
        predefined_acl = "non_default",
        prefix = "x",
        bucket = "x"
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
        verbose = TRUE,
        prefix = "x",
        bucket = "x"
      )
    )
  )
  out <- tar_resources_gcp()
  expect_true(out$verbose)
})

tar_test("tar_resources_gcp() wants a prefix", {
  skip_cran()
  skip_on_os("windows")
  skip_if_not_installed("googleCloudStorageR")
  expect_warning(
    tar_resources_gcp(bucket = "bucket123", prefix = NULL),
    class = "tar_condition_deprecate"
  )
})
