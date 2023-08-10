tar_test("tar_resources_aws()", {
  skip_cran()
  skip_on_os("windows")
  skip_if_not_installed("paws.storage")
  out <- tar_resources_aws(bucket = "bucket123")
  expect_equal(out$bucket, "bucket123")
  expect_null(out$region)
  expect_silent(resources_validate(out))
})

tar_test("tar_resources_aws() with region", {
  skip_cran()
  skip_on_os("windows")
  skip_if_not_installed("paws.storage")
  out <- tar_resources_aws(bucket = "bucket123", region = "us-east-1")
  expect_equal(out$region, "us-east-1")
  expect_silent(resources_validate(out))
})

tar_test("tar_resources_aws() with part_size", {
  skip_cran()
  skip_on_os("windows")
  skip_if_not_installed("paws.storage")
  out <- tar_resources_aws(bucket = "bucket123", part_size = 1e8)
  expect_equal(out$part_size, 1e8)
  expect_silent(resources_validate(out))
})

tar_test("tar_resources_aws() default bucket", {
  skip_cran()
  skip_on_os("windows")
  skip_if_not_installed("paws.storage")
  tar_option_set(
    resources = tar_resources(
      aws = tar_resources_aws(
        bucket = "bucket123"
      )
    )
  )
  out <- tar_resources_aws(part_size = 1e6)
  expect_equal(out$bucket, "bucket123")
  expect_equal(out$part_size, 1e6)
})

tar_test("tar_resources_aws() default prefix", {
  skip_cran()
  skip_on_os("windows")
  skip_if_not_installed("paws.storage")
  tar_option_set(
    resources = tar_resources(
      aws = tar_resources_aws(
        prefix = "my_prefix"
      )
    )
  )
  out <- tar_resources_aws()
  expect_equal(out$prefix, "my_prefix")
})

tar_test("tar_resources_aws() default part_size", {
  skip_cran()
  skip_on_os("windows")
  skip_if_not_installed("paws.storage")
  tar_option_set(
    resources = tar_resources(
      aws = tar_resources_aws(
        part_size = 1e2
      )
    )
  )
  out <- tar_resources_aws()
  expect_equal(out$part_size, 1e2)
})

tar_test("tar_resources_aws() default region", {
  skip_cran()
  skip_on_os("windows")
  skip_if_not_installed("paws.storage")
  tar_option_set(
    resources = tar_resources(
      aws = tar_resources_aws(
        region = "the_moon"
      )
    )
  )
  out <- tar_resources_aws()
  expect_equal(out$region, "the_moon")
})

tar_test("tar_resources_aws() default endpoint", {
  skip_cran()
  skip_on_os("windows")
  skip_if_not_installed("paws.storage")
  tar_option_set(
    resources = tar_resources(
      aws = tar_resources_aws(
        endpoint = "google"
      )
    )
  )
  out <- tar_resources_aws()
  expect_equal(out$endpoint, "google")
})

tar_test("tar_resources_aws() default SSECustomerKey", {
  skip_cran()
  skip_on_os("windows")
  skip_if_not_installed("paws.storage")
  tar_option_set(
    resources = tar_resources(
      aws = tar_resources_aws(
        SSECustomerKey = "x"
      )
    )
  )
  out <- tar_resources_aws()
  expect_equal(out$args$SSECustomerKey, "x")
})
