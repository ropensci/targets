# Use sparingly. We do not want to max out any AWS quotas.
# After this test runs, log into the AWS console,
# check that the data and prefix are correct,
# and MANUALLY CLEAR OUT THE BUCKET.
tar_test("AWS S3 with old resources", {
  skip_if_no_aws()
  bucket_name <- "targets-testing-aws-bucket"
  aws.s3::put_bucket(bucket = "targets-testing-aws-bucket")
  tar_script({
    library(targets)
    library(future)
    tar_option_set(
      format = "aws_rds",
      resources = list(
        bucket = "targets-testing-aws-bucket",
        prefix = "customprefix/customdir"
      )
    )
    list(
      tar_target(a, 1L),
      tar_target(b, a),
      tar_target(c, a + b)
    )
  })
  expect_warning(
    tar_make(callr_function = NULL),
    class = "tar_condition_deprecate"
  )
  expect_equal(tar_read(c), 2L)
})
