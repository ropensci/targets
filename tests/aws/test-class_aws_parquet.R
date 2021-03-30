# Use sparingly. We do not want to max out any AWS quotas.
# And afterwards, manually verify that all the buckets are gone.
tar_test("aws_parquet format returns data frames", {
  skip_if_no_aws()
  skip_if_not_installed("arrow")
  on.exit({
    aws.s3::delete_object(object = "_targets/objects/x", bucket = bucket_name)
    object <- file.path("_targets/objects", tar_meta(y, children)[[1]][1])
    aws.s3::delete_object(object = object, bucket = bucket_name)
    object <- file.path("_targets/objects", tar_meta(y, children)[[1]][2])
    aws.s3::delete_object(object = object, bucket = bucket_name)
    aws.s3::delete_object(object = "_targets/objects", bucket = bucket_name)
    aws.s3::delete_object(object = "_targets", bucket = bucket_name)
    aws.s3::delete_bucket(bucket = bucket_name)
  })
  bucket_name <- random_bucket_name()
  aws.s3::put_bucket(bucket = bucket_name)
  expr <- quote({
    tar_option_set(
      resources = list(bucket = !!bucket_name),
      format = "aws_parquet"
    )
    list(
      tar_target(x, data.frame(x = seq_len(2), y = seq_len(2)))
    )
  })
  expr <- tar_tidy_eval(expr, environment(), TRUE)
  eval(as.call(list(`tar_script`, expr, ask = FALSE)))
  tar_make(callr_function = NULL)
  out <- tar_read(x)
  expect_equal(out, data.frame(x = seq_len(2), y = seq_len(2)))
})
