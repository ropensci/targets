tar_test("aws_rds format", {
  skip("nope")
  skip_if(Sys.getenv("AWS_ACCESS_KEY_ID") == "")
  skip_if(Sys.getenv("AWS_SECRET_ACCESS_KEY") == "")
  skip_if_not_installed("aws.s3")
  skip_if_offline()
  bucket_name <- "file1471b52b7f804"
  on.exit({
    aws.s3::delete_object(object = "_targets/objects/x", bucket = bucket_name)
    aws.s3::delete_object(object = "_targets/objects/y", bucket = bucket_name)
    aws.s3::delete_object(object = "_targets/objects", bucket = bucket_name)
    aws.s3::delete_object(object = "_targets", bucket = bucket_name)
    aws.s3::delete_bucket(bucket = bucket_name)
  })
  aws.s3::put_bucket(bucket = bucket_name)
  expr <- quote({
    tar_option_set(resources = list(bucket = !!bucket_name))
    tar_pipeline(
      tar_target(x, "x_value", format = "aws_rds"),
      tar_target(y, c(x, "y_value"), format = "aws_rds")
    )
  })
  expr <- tidy_eval(expr, environment(), TRUE)
  eval(as.call(list(`tar_script`, expr, ask = FALSE)))
  tar_make(callr_function = NULL)
  expect_equal(tar_read(x), "x_value")
  expect_equal(tar_read(y), c("x_value", "y_value"))
})
