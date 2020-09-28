tar_test("aws_qs format data gets stored", {
  skip_if_no_aws()
  skip_if_not_installed("qs")
  bucket_name <- random_bucket_name()
  aws.s3::put_bucket(bucket = bucket_name)
  expr <- quote({
    tar_option_set(resources = list(bucket = !!bucket_name))
    tar_pipeline(
      tar_target(x, "x_value", format = "aws_qs"),
      tar_target(y, c(x, "y_value"), format = "aws_qs")
    )
  })
  expr <- tidy_eval(expr, environment(), TRUE)
  eval(as.call(list(`tar_script`, expr, ask = FALSE)))
  tar_make(callr_function = NULL)
  expect_true(
    aws.s3::object_exists(bucket = bucket_name, object = "_targets/x")
  )
  expect_true(
    aws.s3::object_exists(bucket = bucket_name, object = "_targets/y")
  )
  expect_false(file.exists(file.path("_targets", "objects", "x")))
  expect_false(file.exists(file.path("_targets", "objects", "y")))
  expect_equal(tar_read(x), "x_value")
  expect_equal(tar_read(y), c("x_value", "y_value"))
  tmp <- tempfile()
  aws.s3::save_object(
    object = "_targets/x",
    bucket = bucket_name,
    file = tmp
  )
  expect_equal(qs::qread(tmp), "x_value")
  aws.s3::delete_object(object = "_targets/x", bucket = bucket_name)
  aws.s3::delete_object(object = "_targets/y", bucket = bucket_name)
  aws.s3::delete_object(object = "_targets", bucket = bucket_name)
  aws.s3::delete_bucket(bucket = bucket_name)
  expect_false(aws.s3::bucket_exists(bucket = bucket_name))
})

tar_test("aws_qs format invalidation", {
  skip_if_no_aws()
  skip_if_not_installed("qs")
  bucket_name <- random_bucket_name()
  aws.s3::put_bucket(bucket = bucket_name)
  expr <- quote({
    tar_option_set(resources = list(bucket = !!bucket_name))
    tar_pipeline(
      tar_target(x, "x_value", format = "aws_qs"),
      tar_target(y, c(x, "y_value"), format = "aws_qs")
    )
  })
  expr <- tidy_eval(expr, environment(), TRUE)
  eval(as.call(list(`tar_script`, expr, ask = FALSE)))
  tar_make(callr_function = NULL)
  expect_equal(tar_progress(x)$progress, "built")
  expect_equal(tar_progress(y)$progress, "built")
  tar_make(callr_function = NULL)
  expect_equal(nrow(tar_progress()), 0L)
  expr <- quote({
    tar_option_set(resources = list(bucket = !!bucket_name))
    tar_pipeline(
      tar_target(x, "x_value2", format = "aws_qs"),
      tar_target(y, c(x, "y_value"), format = "aws_qs")
    )
  })
  expr <- tidy_eval(expr, environment(), TRUE)
  eval(as.call(list(`tar_script`, expr, ask = FALSE)))
  tar_make(callr_function = NULL)
  expect_equal(tar_progress(x)$progress, "built")
  expect_equal(tar_progress(y)$progress, "built")
  expect_equal(tar_read(x), "x_value2")
  expect_equal(tar_read(y), c("x_value2", "y_value"))
  aws.s3::delete_object(object = "_targets/x", bucket = bucket_name)
  aws.s3::delete_object(object = "_targets/y", bucket = bucket_name)
  aws.s3::delete_object(object = "_targets", bucket = bucket_name)
  aws.s3::delete_bucket(bucket = bucket_name)
  expect_false(aws.s3::bucket_exists(bucket = bucket_name))
})
