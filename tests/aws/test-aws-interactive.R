# Run these tests interactively line by line.
# There are things to be done in the AWS web console
# in the middle of the tests. Follow the directions
# in the comments.
# As with other AWS tests, use sparingly.
# We do not want to max out any AWS quotas.
# And afterwards, manually verify that all the buckets are gone.
tar_test("aws_qs nonexistent bucket", {
  skip_if_no_aws()
  skip_if_not_installed("qs")
  bucket_name <- random_bucket_name()
  aws.s3::put_bucket(bucket = bucket_name)
  expr <- quote({
    tar_option_set(
      resources = tar_resources(
        aws = tar_resources_aws(bucket = !!bucket_name, region = "")
      )
    )
    list(
      tar_target(x, "x_value", format = "aws_qs"),
      tar_target(y, c(x, "y_value"), format = "aws_qs")
    )
  })
  expr <- tar_tidy_eval(expr, environment(), TRUE)
  eval(as.call(list(`tar_script`, expr, ask = FALSE)))
  tar_make(callr_function = NULL)
  expect_equal(tar_read(x), "x_value")
  # Now delete the bucket in the AWS console and make sure it's gone.
  expect_error(
    tar_make(callr_function = NULL, reporter = "silent"),
    class = "tar_condition_run"
  )
  out <- tar_meta(x, error)$error
  expect_true(nzchar(out))
  expect_false(anyNA(out))
})
