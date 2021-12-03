tar_test("aws_exists()", {
  bucket <- random_bucket_name()
  paws::s3()$create_bucket(Bucket = bucket)
  on.exit({
    paws::s3()$delete_object(Key = "x", Bucket = bucket)
    paws::s3()$delete_bucket(Bucket = bucket)
  })
  expect_false(aws_exists(key = "x", bucket = bucket))
  tmp <- tempfile()
  writeLines("x", tmp)
  paws::s3()$put_object(Body = tmp, Key = "x", Bucket = bucket)
  expect_true(aws_exists(key = "x", bucket = bucket))
})

tar_test("aws_head()", {
  bucket <- random_bucket_name()
  paws::s3()$create_bucket(Bucket = bucket)
  on.exit({
    paws::s3()$delete_object(Key = "x", Bucket = bucket)
    paws::s3()$delete_bucket(Bucket = bucket)
  })
  tmp <- tempfile()
  writeLines("x", tmp)
  paws::s3()$put_object(Body = tmp, Key = "x", Bucket = bucket)
  head <- aws_head(key = "x", bucket = bucket)
  expect_true(is.list(head))
  expect_true(is.character(head$ETag))
  expect_true(nzchar(head$ETag))
})

tar_test("aws_download()", {
  bucket <- random_bucket_name()
  paws::s3()$create_bucket(Bucket = bucket)
  on.exit({
    paws::s3()$delete_object(Key = "x", Bucket = bucket)
    paws::s3()$delete_bucket(Bucket = bucket)
  })
  tmp <- tempfile()
  writeLines("x", tmp)
  paws::s3()$put_object(Body = tmp, Key = "x", Bucket = bucket)
  tmp2 <- tempfile()
  expect_false(file.exists(tmp2))
  aws_download(file = tmp2, key = "x", bucket = bucket)
  expect_equal(readLines(tmp2), "x")
})

tar_test("aws_upload() without headers", {
  bucket <- random_bucket_name()
  paws::s3()$create_bucket(Bucket = bucket)
  on.exit({
    paws::s3()$delete_object(Key = "x", Bucket = bucket)
    paws::s3()$delete_bucket(Bucket = bucket)
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

tar_test("aws_upload() and download with metadata and region", {
  bucket <- random_bucket_name()
  region <- "us-west-2"
  old <- Sys.getenv("AWS_REGION")
  Sys.setenv(AWS_REGION = region)
  paws::s3()$create_bucket(
    Bucket = bucket,
    CreateBucketConfiguration = list(LocationConstraint = region)
  )
  Sys.setenv(AWS_REGION = old)
  on.exit({
    paws::s3()$delete_object(Key = "x", Bucket = bucket)
    paws::s3()$delete_bucket(Bucket = bucket)
  })
  expect_false(aws_exists(key = "x", bucket = bucket, region = region))
  tmp <- tempfile()
  writeLines("x", tmp)
  aws_upload(
    file = tmp,
    key = "x",
    bucket = bucket,
    metadata = list("custom" = "custom_metadata"),
    region = region
  )
  expect_true(aws_exists(key = "x", bucket = bucket, region = region))
  head <- aws_head(key = "x", bucket = bucket, region = region)
  expect_equal(head$Metadata$custom, "custom_metadata")
  tmp2 <- tempfile()
  expect_false(file.exists(tmp2))
  aws_download(file = tmp2, key = "x", bucket = bucket, region = region)
  expect_equal(readLines(tmp2), "x")
})

tar_test("upload twice, get the correct version", {
  # setup
  # aws_upload() 2 different objects with different metadata and same keys
  # aws_exists() old, new, and nonexistant
  # aws_head() old, new, and nonexistent. check metadata and version.
  # aws_download() old, new, and nonexistent. check data
})

tar_test("multipart: upload twice, get the correct version", {
  # setup
  # aws_upload() 2 different multipart objects with diff metadata & same keys
  # aws_exists() old, new, and nonexistant
  # aws_head() old, new, and nonexistent. check metadata and version.
  # aws_download() old, new, and nonexistent. check data
})
