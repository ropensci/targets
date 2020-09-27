tar_test("aws_rds format", {
  skip_if(Sys.getenv("AWS_ACCESS_KEY_ID") == "")
  skip_if(Sys.getenv("AWS_SECRET_ACCESS_KEY") == "")
  skip_if_not_installed("aws.s3")
  bucket_name <- "file1471b52b7f804"
  on.exit(aws.s3::delete_bucket(bucket = bucket_name))
  aws.s3::put_bucket(bucket = bucket_name)
  tar_script({
    tar_option_set(resources = list(bucket = "file1471b52b7f804"))
    tar_pipeline(
      tar_target(x, "x_value", format = "aws_rds"),
      tar_target(y, c(x, "y_value"), format = "aws_rds")
    )
  })
  tar_make(callr_function = NULL)
})
