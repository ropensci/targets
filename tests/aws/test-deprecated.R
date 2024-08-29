# Use sparingly to minimize AWS costs.
# Verify all `targets` buckets are deleted afterwards.
# After this test runs, log into the AWS console,
# check that the data and prefix are correct,
# and MANUALLY CLEAR OUT THE BUCKET.
tar_test("deprecated format = \"aws_parquet\"", {
  skip_if_no_aws()
  skip_if_not_installed("arrow")
  s3 <- paws.storage::s3()
  bucket_name <- random_bucket_name()
  s3$create_bucket(Bucket = bucket_name)
  on.exit(aws_s3_delete_bucket(bucket_name))
  expr <- quote({
    tar_option_set(
      resources = tar_resources(
        aws = tar_resources_aws(bucket = !!bucket_name, prefix = "_targets")
      ),
      format = "aws_parquet"
    )
    list(
      tar_target(x, data.frame(x = seq_len(2), y = seq_len(2)))
    )
  })
  expr <- tar_tidy_eval(expr, environment(), TRUE)
  eval(as.call(list(`tar_script`, expr, ask = FALSE)))
  suppressWarnings(
    expect_warning(
      tar_make(callr_function = NULL),
      class = "tar_condition_deprecate"
    )
  )
  out <- as.data.frame(tar_read(x))
  expect_equal(out, data.frame(x = seq_len(2), y = seq_len(2)))
})

tar_test("migrate meta database", {
  skip_if_no_aws()
  skip_if_not_installed("arrow")
  s3 <- paws.storage::s3()
  bucket_name <- random_bucket_name()
  s3$create_bucket(Bucket = bucket_name)
  on.exit(aws_s3_delete_bucket(bucket_name))
  expr <- quote({
    tar_option_set(
      resources = tar_resources(
        aws = tar_resources_aws(bucket = !!bucket_name, prefix = "_targets")
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
  tar_make(callr_function = NULL)
  expect_equal(tar_outdated(callr_function = NULL), character(0))
  meta <- meta_init()
  data <- as_data_frame(meta$database$read_condensed_data())
  expect_false(is.null(data$repository))
  data$repository <- NULL
  expect_null(data$repository)
  data$format[!is.na(data$format)] <- "aws_parquet"
  meta$database$overwrite_storage(data)
  expect_equal(tar_outdated(callr_function = NULL), character(0))
  out <- as.data.frame(tar_read(x))
  expect_equal(out, data.frame(x = seq_len(2), y = seq_len(2)))
})
