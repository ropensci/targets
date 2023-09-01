# Switch to GCP HMAC versions of AWS env vars.
# Use sparingly to minimize AWS costs.
# And afterwards, manually verify that all the buckets are gone.
endpoint <- "https://storage.googleapis.com"
region <- "auto"

client <- function() {
  paws.storage::s3(
    config = list(
      endpoint = endpoint,
      region = region
    )
  )
}

tar_test("aws qs format data gets stored", {
  skip_if_no_aws()
  skip_if_no_gcp()
  skip_if_not_installed("qs")
  s3 <- client()
  bucket_name <- random_bucket_name()
  on.exit(aws_s3_delete_bucket(bucket_name, client()))
  s3$create_bucket(Bucket = bucket_name)
  expr <- quote({
    tar_option_set(
      resources = tar_resources(
        aws = tar_resources_aws(
          bucket = !!bucket_name,
          region = !!region,
          endpoint = !!endpoint,
          prefix = "_targets"
        )
      )
    )
    list(
      tar_target(x, "x_value", format = "qs", repository = "aws"),
      tar_target(y, c(x, "y_value"), format = "qs", repository = "aws")
    )
  })
  expr <- tar_tidy_eval(expr, environment(), TRUE)
  eval(as.call(list(`tar_script`, expr, ask = FALSE)))
  tar_make(callr_function = NULL)
  expect_true(
    aws_s3_exists(
      bucket = bucket_name,
      key = "_targets/objects/x",
      region = region,
      endpoint = endpoint
    )
  )
  expect_true(
    aws_s3_exists(
      bucket = bucket_name,
      key = "_targets/objects/y",
      region = region,
      endpoint = endpoint
    )
  )
  expect_false(file.exists(file.path("_targets", "objects", "x")))
  expect_false(file.exists(file.path("_targets", "objects", "y")))
  expect_equal(tar_read(x), "x_value")
  expect_equal(tar_read(y), c("x_value", "y_value"))
  tmp <- tempfile()
  aws_s3_download(
    key = "_targets/objects/x",
    bucket = bucket_name,
    file = tmp,
    region = region,
    endpoint = endpoint
  )
  expect_equal(qs::qread(tmp), "x_value")
})

tar_test("aws qs format invalidation", {
  skip_if_no_aws()
  skip_if_no_gcp()
  skip_if_not_installed("qs")
  s3 <- client()
  bucket_name <- random_bucket_name()
  s3$create_bucket(Bucket = bucket_name)
  on.exit(aws_s3_delete_bucket(bucket_name, client()))
  expr <- quote({
    tar_option_set(
      resources = tar_resources(
        aws = tar_resources_aws(
          bucket = !!bucket_name,
          region = !!region,
          endpoint = !!endpoint,
          prefix = "_targets"
        )
      )
    )
    list(
      tar_target(x, "x_value", format = "qs", repository = "aws"),
      tar_target(y, c(x, "y_value"), format = "qs", repository = "aws")
    )
  })
  expr <- tar_tidy_eval(expr, environment(), TRUE)
  eval(as.call(list(`tar_script`, expr, ask = FALSE)))
  tar_make(callr_function = NULL)
  expect_equal(tar_progress(x)$progress, "built")
  expect_equal(tar_progress(y)$progress, "built")
  tar_make(callr_function = NULL)
  progress <- tar_progress()
  progress <- progress[progress$progress != "skipped", ]
  expect_equal(nrow(progress), 0L)
  expr <- quote({
    tar_option_set(
      resources = tar_resources(
        aws = tar_resources_aws(
          bucket = !!bucket_name,
          region = !!region,
          endpoint = !!endpoint,
          prefix = "_targets"
        )
      )
    )
    list(
      tar_target(x, "x_value2", format = "qs", repository = "aws"),
      tar_target(y, c(x, "y_value"), format = "qs", repository = "aws")
    )
  })
  expr <- tar_tidy_eval(expr, environment(), TRUE)
  eval(as.call(list(`tar_script`, expr, ask = FALSE)))
  tar_make(callr_function = NULL)
  expect_equal(tar_progress(x)$progress, "built")
  expect_equal(tar_progress(y)$progress, "built")
  expect_equal(tar_read(x), "x_value2")
  expect_equal(tar_read(y), c("x_value2", "y_value"))
})
