# Use sparingly. We do not want to max out any AWS quotas.
# And afterwards, manually verify that all the buckets are gone.
tar_test("aws_parquet format returns data frames", {
  skip_if_no_aws()
  skip_if_not_installed("arrow")
  s3 <- paws::s3()
  on.exit({
    s3$delete_object(Key = "_targets/objects/x", Bucket = bucket_name)
    object <- file.path("_targets/objects", tar_meta(y, children)[[1]][1])
    s3$delete_object(Key = object, Bucket = bucket_name)
    object <- file.path("_targets/objects", tar_meta(y, children)[[1]][2])
    s3$delete_object(Key = object, Bucket = bucket_name)
    s3$delete_object(Key = "_targets/objects", Bucket = bucket_name)
    s3$delete_object(Key = "_targets", Bucket = bucket_name)
    s3$delete_bucket(Bucket = bucket_name)
  })
  bucket_name <- random_bucket_name()
  s3$create_bucket(Bucket = bucket_name)
  expr <- quote({
    tar_option_set(
      resources = tar_resources(
        aws = tar_resources_aws(bucket = !!bucket_name)
      ),
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
