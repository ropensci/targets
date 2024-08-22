tar_test("tar_resources_aws()", {
  skip_cran()
  skip_on_os("windows")
  skip_if_not_installed("paws.storage")
  out <- tar_resources_aws(bucket = "bucket123", prefix = "x")
  expect_equal(out$bucket, "bucket123")
  expect_null(out$region)
  expect_silent(resources_validate(out))
})

tar_test("tar_resources_aws() with region", {
  skip_cran()
  skip_on_os("windows")
  skip_if_not_installed("paws.storage")
  out <- tar_resources_aws(
    bucket = "bucket123",
    region = "us-east-1",
    prefix = "x"
  )
  expect_equal(out$region, "us-east-1")
  expect_silent(resources_validate(out))
})

tar_test("tar_resources_aws() with part_size", {
  skip_cran()
  skip_on_os("windows")
  skip_if_not_installed("paws.storage")
  out <- tar_resources_aws(
    bucket = "bucket123",
    part_size = 1e8,
    prefix = "x"
  )
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
        bucket = "bucket123",
        prefix = "x"
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
        bucket = "x",
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
        part_size = 1e2,
        prefix = "x",
        bucket = "x"
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
        region = "the_moon",
        prefix = "x",
        bucket = "x"
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
        endpoint = "google",
        prefix = "x",
        bucket = "x"
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
        SSECustomerKey = "x",
        prefix = "x",
        bucket = "x"
      )
    )
  )
  out <- tar_resources_aws()
  expect_equal(out$args$SSECustomerKey, "x")
})

tar_test("tar_resources_aws() default seconds_timeout", {
  skip_cran()
  skip_on_os("windows")
  skip_if_not_installed("paws.storage")
  tar_option_set(
    resources = tar_resources(
      aws = tar_resources_aws(
        seconds_timeout = 2.1,
        prefix = "x",
        bucket = "x"
      )
    )
  )
  out <- tar_resources_aws()
  expect_equal(out$seconds_timeout, 2.1)
})

tar_test("tar_resources_aws() default close_connection", {
  skip_cran()
  skip_on_os("windows")
  skip_if_not_installed("paws.storage")
  tar_option_set(
    resources = tar_resources(
      aws = tar_resources_aws(
        close_connection = FALSE,
        prefix = "x",
        bucket = "x"
      )
    )
  )
  out <- tar_resources_aws()
  expect_false(out$close_connection)
})

tar_test("tar_resources_aws() default s3_force_path_style", {
  skip_cran()
  skip_on_os("windows")
  skip_if_not_installed("paws.storage")
  tar_option_set(
    resources = tar_resources(
      aws = tar_resources_aws(
        s3_force_path_style = TRUE,
        prefix = "x",
        bucket = "x"
      )
    )
  )
  out <- tar_resources_aws()
  expect_true(out$s3_force_path_style)
})

tar_test("tar_resources_aws() wants a prefix", {
  skip_cran()
  skip_on_os("windows")
  skip_if_not_installed("paws.storage")
  expect_warning(
    tar_resources_aws(bucket = "bucket123", prefix = NULL),
    class = "tar_condition_deprecate"
  )
})

tar_test("tar_resources_aws() verbose", {
  skip_cran()
  skip_on_os("windows")
  skip_if_not_installed("paws.storage")
  tar_option_set(
    resources = tar_resources(
      aws = tar_resources_aws(
        prefix = "x",
        bucket = "x"
      )
    )
  )
  out <- tar_resources_aws()
  expect_true(out$verbose)
  tar_option_set(
    resources = tar_resources(
      aws = tar_resources_aws(
        verbose = FALSE,
        prefix = "x",
        bucket = "x"
      )
    )
  )
  out <- tar_resources_aws()
  expect_false(out$verbose)
})

tar_test("tar_resources_aws() page_size", {
  skip_cran()
  skip_on_os("windows")
  skip_if_not_installed("paws.storage")
  tar_option_set(
    resources = tar_resources(
      aws = tar_resources_aws(
        prefix = "x",
        bucket = "x"
      )
    )
  )
  out <- tar_resources_aws()
  expect_equal(out$page_size, 1000L)
  tar_option_set(
    resources = tar_resources(
      aws = tar_resources_aws(
        page_size = 3L,
        prefix = "x",
        bucket = "x"
      )
    )
  )
  out <- tar_resources_aws()
  expect_equal(out$page_size, 3L)
})
