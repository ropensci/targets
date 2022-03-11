# Use sparingly. We do not want to max out any AWS quotas.
# And afterwards, manually verify that all the buckets are gone.
tar_test("s3_config option gets noticed (should fail)", {
  skip_if_no_aws()
  skip_if_not_installed("qs")
  s3 <- paws::s3()
  bucket_name <- random_bucket_name()
  on.exit(destroy_bucket(bucket_name))
  s3$create_bucket(Bucket = bucket_name)
  expr <- quote({
    tar_option_set(
      resources = tar_resources(
        aws = tar_resources_aws(bucket = !!bucket_name)
      )
    )
    tar_option_set(s3_config = list(endpoint = "invalid"))
    list(
      tar_target(x, "x_value", format = "aws_qs"),
      tar_target(y, c(x, "y_value"), format = "aws_qs")
    )
  })
  expr <- tar_tidy_eval(expr, environment(), TRUE)
  eval(as.call(list(`tar_script`, expr, ask = FALSE)))
  expect_error(tar_make(callr_function = NULL), class = "tar_condition_run")
})
