# Use sparingly to minimize AWS costs.
# Verify all `targets` buckets are deleted afterwards.
tar_test("aws_parquet format returns data frames", {
  skip_if_no_aws()
  skip_if_not_installed("arrow")
  s3 <- paws.storage::s3()
  bucket_name <- random_bucket_name()
  s3$create_bucket(Bucket = bucket_name)
  on.exit(aws_s3_delete_bucket(bucket_name))
  expr <- quote({
    tar_option_set(
      resources = tar_resources(
        aws = tar_resources_aws(
          bucket = !!bucket_name,
          ExpectedBucketOwner = "phantom_f4acd87c52d4e62b",
          prefix = "_targets"
        )
      ),
      format = "parquet",
      repository = "aws"
    )
    list(
      tar_target(x, data.frame(x = seq_len(2), y = seq_len(2)))
    )
  })
  expr <- tar_tidy_eval(expr, environment(), TRUE)
  eval(as.call(list(`tar_script`, expr, ask = FALSE)))
  expect_silent(tar_manifest(callr_function = NULL))
  expect_error(
    tar_make(callr_function = NULL),
    class = "tar_condition_run"
  )
})
