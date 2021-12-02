tar_test("aws_exists()", {
  bucket <- random_bucket_name()
  aws.s3::put_bucket(bucket)
  on.exit({
    aws.s3::delete_object(object = "x", bucket = bucket)
    aws.s3::delete_bucket(bucket = bucket)
  })
  expect_false(aws_exists(key = "x", bucket = bucket))
  tmp <- tempfile()
  writeLines("x", tmp)
  aws.s3::put_object(file = tmp, object = "x", bucket = bucket)
  expect_true(aws_exists(key = "x", bucket = bucket))
})

tar_test("aws_head()", {
  bucket <- random_bucket_name()
  aws.s3::put_bucket(bucket)
  on.exit({
    aws.s3::delete_object(object = "x", bucket = bucket)
    aws.s3::delete_bucket(bucket = bucket)
  })
  tmp <- tempfile()
  writeLines("x", tmp)
  aws.s3::put_object(file = tmp, object = "x", bucket = bucket)
  head <- aws_head(key = "x", bucket = bucket)
  expect_true(is.logical(head))
  expect_true(is.character(attr(head, "etag")))
  expect_true(nzchar(attr(head, "etag")))
})

tar_test("aws_download()", {
  bucket <- random_bucket_name()
  aws.s3::put_bucket(bucket)
  on.exit({
    aws.s3::delete_object(object = "x", bucket = bucket)
    aws.s3::delete_bucket(bucket = bucket)
  })
  tmp <- tempfile()
  writeLines("x", tmp)
  aws.s3::put_object(file = tmp, object = "x", bucket = bucket)
  tmp2 <- tempfile()
  expect_false(file.exists(tmp2))
  aws_download(file = tmp2, key = "x", bucket = bucket)
  expect_equal(readLines(tmp2), "x")
})

tar_test("aws_upload() without headers", {
  bucket <- random_bucket_name()
  aws.s3::put_bucket(bucket)
  on.exit({
    aws.s3::delete_object(object = "x", bucket = bucket)
    aws.s3::delete_bucket(bucket = bucket)
  })
  expect_false(aws_exists(key = "x", bucket = bucket))
  tmp <- tempfile()
  writeLines("x", tmp)
  aws_upload(
    file = tmp,
    key = "x",
    bucket = bucket
  )
  expect_true(aws_exists(key = "x", bucket = bucket))
})

tar_test("aws_upload() and download with headers and region", {
  bucket <- random_bucket_name()
  region <- "us-west-2"
  aws.s3::put_bucket(bucket, region = region)
  on.exit({
    aws.s3::delete_object(object = "x", bucket = bucket)
    aws.s3::delete_bucket(bucket = bucket, region = region)
  })
  expect_false(aws_exists(key = "x", bucket = bucket, region = region))
  tmp <- tempfile()
  writeLines("x", tmp)
  aws_upload(
    file = tmp,
    key = "x",
    bucket = bucket,
    headers = c("x-amz-meta-custom" = "custom_metadata"),
    region = region
  )
  expect_true(aws_exists(key = "x", bucket = bucket, region = region))
  head <- aws_head(key = "x", bucket = bucket, region = region)
  expect_equal(attr(head, "x-amz-meta-custom"), "custom_metadata")
  tmp2 <- tempfile()
  expect_false(file.exists(tmp2))
  aws_download(file = tmp2, key = "x", bucket = bucket, region = region)
  expect_equal(readLines(tmp2), "x")
})

tar_test("aws_version()", {
  bucket <- random_bucket_name()
  aws.s3::put_bucket(bucket)
  on.exit({
    aws.s3::delete_object(object = "x", bucket = bucket)
    aws.s3::delete_bucket(bucket = bucket)
  })
  aws.s3::put_versioning(bucket, "Enabled")
  tmp <- tempfile()
  writeLines("x", tmp)
  aws_upload(
    file = tmp,
    key = "x",
    bucket = bucket
  )
  version <- aws_version(key = "x", bucket = bucket)
  expect_true(is.character(version))
  expect_true(nzchar(version))
})
