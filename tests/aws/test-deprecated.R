# Use sparingly. We do not want to max out any AWS quotas.
# After this test runs, log into the AWS console,
# check that the data and prefix are correct,
# and MANUALLY CLEAR OUT THE BUCKET.
tar_test("AWS S3 with old resources", {
  skip_if_no_aws()
  bucket_name <- random_bucket_name()
  s3 <- paws::s3()
  s3$create_bucket(Bucket = bucket_name)
  on.exit(aws_s3_delete_bucket(bucket_name))
  code <- substitute({
    library(targets)
    library(future)
    tar_option_set(
      format = "rds",
      repository = "aws",
      resources = list(
        bucket = bucket_name,
        prefix = "customprefix/customdir"
      )
    )
    list(
      tar_target(a, 1L),
      tar_target(b, a),
      tar_target(c, a + b)
    )
  }, env = list(bucket_name = bucket_name))
  do.call(tar_script, list(code = code))
  expect_warning(
    tar_make(callr_function = NULL),
    class = "tar_condition_deprecate"
  )
  expect_equal(tar_read(c), 2L)
})

tar_test("deprecated format = \"aws_parquet\"", {
  skip_if_no_aws()
  skip_if_not_installed("arrow")
  s3 <- paws::s3()
  bucket_name <- random_bucket_name()
  s3$create_bucket(Bucket = bucket_name)
  on.exit(aws_s3_delete_bucket(bucket_name))
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
  expect_warning(
    tar_make(callr_function = NULL),
    class = "tar_condition_deprecate"
  )
  out <- tar_read(x)
  expect_equal(out, data.frame(x = seq_len(2), y = seq_len(2)))
})


tar_test("migrate meta database", {
  skip_if_no_aws()
  skip_if_not_installed("arrow")
  s3 <- paws::s3()
  bucket_name <- random_bucket_name()
  s3$create_bucket(Bucket = bucket_name)
  on.exit(aws_s3_delete_bucket(bucket_name))
  expr <- quote({
    tar_option_set(
      resources = tar_resources(
        aws = tar_resources_aws(bucket = !!bucket_name)
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
  expect_true(is.null(data$repository))
  data$format[!is.na(data$format)] <- "aws_parquet"
  meta$database$overwrite_storage(data)
  expect_equal(tar_outdated(callr_function = NULL), character(0))
  out <- tar_read(x)
  expect_equal(out, data.frame(x = seq_len(2), y = seq_len(2)))
})
