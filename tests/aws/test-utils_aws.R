# Use sparingly to minimize AWS costs.
# Verify all `targets` buckets are deleted afterwards.
tar_test("aws_s3_exists()", {
  bucket <- random_bucket_name()
  paws.storage::s3()$create_bucket(Bucket = bucket)
  on.exit(aws_s3_delete_bucket(bucket))
  expect_false(aws_s3_exists(key = "x", bucket = bucket))
  tmp <- tempfile()
  writeLines("x", tmp)
  paws.storage::s3()$put_object(Body = tmp, Key = "x", Bucket = bucket)
  expect_true(aws_s3_exists(key = "x", bucket = bucket))
  expect_false(
    aws_s3_exists(
      key = "x",
      bucket = bucket,
      args = list(ExpectedBucketOwner = "phantom_f4acd87c52d4e62b")
    )
  )
})

tar_test("aws_s3_head()", {
  bucket <- random_bucket_name()
  paws.storage::s3()$create_bucket(Bucket = bucket)
  on.exit(aws_s3_delete_bucket(bucket))
  expect_null(aws_s3_head(key = "x", bucket = random_bucket_name()))
  expect_null(aws_s3_head(key = "x", bucket = bucket))
  tmp <- tempfile()
  writeLines("x", tmp)
  paws.storage::s3()$put_object(Body = tmp, Key = "x", Bucket = bucket)
  head <- aws_s3_head(key = "x", bucket = bucket)
  expect_true(is.list(head))
  expect_true(is.character(head$ETag))
  expect_true(nzchar(head$ETag))
  expect_null(
    aws_s3_head(
      key = "x",
      bucket = bucket,
      args = list(ExpectedBucketOwner = "phantom_f4acd87c52d4e62b")
    )
  )
})

tar_test("aws_s3_download()", {
  bucket <- random_bucket_name()
  paws.storage::s3()$create_bucket(Bucket = bucket)
  on.exit(aws_s3_delete_bucket(bucket))
  tmp <- tempfile()
  writeLines("x", tmp)
  paws.storage::s3()$put_object(Body = tmp, Key = "x", Bucket = bucket)
  tmp2 <- tempfile()
  expect_false(file.exists(tmp2))
  aws_s3_download(file = tmp2, key = "x", bucket = bucket)
  expect_error(
    aws_s3_download(
      file = tmp2,
      key = "x",
      bucket = bucket,
      args = list(ExpectedBucketOwner = "phantom_f4acd87c52d4e62b")
    ),
    class = "http_error"
  )
  expect_equal(readLines(tmp2), "x")
})

tar_test("aws_s3_delete()", {
  bucket <- random_bucket_name()
  paws.storage::s3()$create_bucket(Bucket = bucket)
  on.exit(aws_s3_delete_bucket(bucket))
  tmp <- tempfile()
  writeLines("x", tmp)
  key <- "x"
  paws.storage::s3()$put_object(Body = tmp, Key = key, Bucket = bucket)
  expect_true(aws_s3_exists(key = key, bucket = bucket))
  expect_error(
    aws_s3_delete(
      key = key,
      bucket = bucket,
      args = list(ExpectedBucketOwner = "phantom_f4acd87c52d4e62b")
    ),
    class = "http_error"
  )
  expect_true(aws_s3_exists(key = key, bucket = bucket))
  aws_s3_delete(key = key, bucket = bucket)
  expect_false(aws_s3_exists(key = key, bucket = bucket))
  aws_s3_delete(key = key, bucket = bucket)
})

tar_test("aws_s3_delete() version", {
  bucket <- random_bucket_name()
  paws.storage::s3()$create_bucket(Bucket = bucket)
  paws.storage::s3()$put_bucket_versioning(
    Bucket = bucket,
    VersioningConfiguration = list(
      MFADelete = "Disabled",
      Status = "Enabled"
    )
  )
  on.exit(aws_s3_delete_bucket(bucket))
  tmp <- tempfile()
  writeLines("x", tmp)
  key <- "x"
  head1 <- paws.storage::s3()$put_object(Body = tmp, Key = key, Bucket = bucket)
  version1 <- head1$VersionId
  writeLines("y", tmp)
  head2 <- paws.storage::s3()$put_object(Body = tmp, Key = key, Bucket = bucket)
  version2 <- head2$VersionId
  expect_true(aws_s3_exists(key = key, bucket = bucket, version = version1))
  expect_true(aws_s3_exists(key = key, bucket = bucket, version = version2))
  aws_s3_delete(key = key, bucket = bucket, version = version1)
  expect_false(aws_s3_exists(key = key, bucket = bucket, version = version1))
  expect_true(aws_s3_exists(key = key, bucket = bucket, version = version2))
})

tar_test("aws_s3_upload() without headers", {
  bucket <- random_bucket_name()
  paws.storage::s3()$create_bucket(Bucket = bucket)
  on.exit(aws_s3_delete_bucket(bucket))
  expect_false(aws_s3_exists(key = "x", bucket = bucket))
  tmp <- tempfile()
  writeLines("x", tmp)
  expect_error(
    aws_s3_upload(
      file = tmp,
      key = "x",
      bucket = bucket,
      args = list(ExpectedBucketOwner = "phantom_f4acd87c52d4e62b")
    ),
    class = "http_error"
  )
  expect_false(aws_s3_exists(key = "x", bucket = bucket))
  aws_s3_upload(
    file = tmp,
    key = "x",
    bucket = bucket
  )
  expect_true(aws_s3_exists(key = "x", bucket = bucket))
})

tar_test("aws_s3_upload() and download with metadata and region", {
  bucket <- random_bucket_name()
  region <- "us-west-2"
  paws.storage::s3()$create_bucket(
    Bucket = bucket,
    CreateBucketConfiguration = list(LocationConstraint = region)
  )
  on.exit(aws_s3_delete_bucket(bucket))
  expect_false(aws_s3_exists(key = "x", bucket = bucket, region = region))
  tmp <- tempfile()
  writeLines("x", tmp)
  aws_s3_upload(
    file = tmp,
    key = "x",
    bucket = bucket,
    metadata = list("custom" = "custom_metadata"),
    region = region
  )
  expect_true(aws_s3_exists(key = "x", bucket = bucket, region = region))
  head <- aws_s3_head(key = "x", bucket = bucket, region = region)
  expect_equal(head$Metadata$custom, "custom_metadata")
  tmp2 <- tempfile()
  expect_false(file.exists(tmp2))
  aws_s3_download(file = tmp2, key = "x", bucket = bucket, region = region)
  expect_equal(readLines(tmp2), "x")
})

tar_test("upload twice, get the correct version", {
  bucket <- random_bucket_name()
  paws.storage::s3()$create_bucket(Bucket = bucket)
  on.exit(aws_s3_delete_bucket(bucket))
  paws.storage::s3()$put_bucket_versioning(
    Bucket = bucket,
    VersioningConfiguration = list(
      MFADelete = "Disabled",
      Status = "Enabled"
    )
  )
  tmp <- tempfile()
  writeLines("first", tmp)
  head_first <- aws_s3_upload(
    file = tmp,
    key = "x",
    bucket = bucket,
    metadata = list("custom" = "first-meta")
  )
  v1 <- head_first$VersionId
  writeLines("second", tmp)
  head_second <- aws_s3_upload(
    file = tmp,
    key = "x",
    bucket = bucket,
    metadata = list("custom" = "second-meta")
  )
  v2 <- head_second$VersionId
  expect_true(aws_s3_exists(key = "x", bucket = bucket))
  expect_true(aws_s3_exists(key = "x", bucket = bucket, version = v1))
  expect_true(aws_s3_exists(key = "x", bucket = bucket, version = v2))
  expect_false(aws_s3_exists(key = "x", bucket = bucket, version = "v3"))
  h1 <- aws_s3_head(key = "x", bucket = bucket, version = v1)
  h2 <- aws_s3_head(key = "x", bucket = bucket, version = v2)
  expect_equal(h1$VersionId, v1)
  expect_equal(h2$VersionId, v2)
  expect_equal(h1$Metadata$custom, "first-meta")
  expect_equal(h2$Metadata$custom, "second-meta")
  unlink(tmp)
  aws_s3_download(file = tmp, key = "x", bucket = bucket, version = v1)
  expect_equal(readLines(tmp), "first")
  aws_s3_download(file = tmp, key = "x", bucket = bucket, version = v2)
  expect_equal(readLines(tmp), "second")
})

tar_test("multipart: upload twice, get the correct version", {
  bucket <- random_bucket_name()
  paws.storage::s3()$create_bucket(Bucket = bucket)
  on.exit(aws_s3_delete_bucket(bucket))
  paws.storage::s3()$put_bucket_versioning(
    Bucket = bucket,
    VersioningConfiguration = list(
      MFADelete = "Disabled",
      Status = "Enabled"
    )
  )
  tmp <- tempfile()
  writeLines("first", tmp)
  head_first <- aws_s3_upload(
    file = tmp,
    key = "x",
    bucket = bucket,
    multipart = TRUE,
    metadata = list("custom" = "first-meta")
  )
  v1 <- head_first$VersionId
  writeLines("second", tmp)
  head_second <- aws_s3_upload(
    file = tmp,
    key = "x",
    bucket = bucket,
    multipart = TRUE,
    metadata = list("custom" = "second-meta")
  )
  v2 <- head_second$VersionId
  expect_true(aws_s3_exists(key = "x", bucket = bucket))
  expect_true(aws_s3_exists(key = "x", bucket = bucket, version = v1))
  expect_true(aws_s3_exists(key = "x", bucket = bucket, version = v2))
  expect_false(aws_s3_exists(key = "x", bucket = bucket, version = "v3"))
  h1 <- aws_s3_head(key = "x", bucket = bucket, version = v1)
  h2 <- aws_s3_head(key = "x", bucket = bucket, version = v2)
  expect_equal(h1$VersionId, v1)
  expect_equal(h2$VersionId, v2)
  expect_equal(h1$Metadata$custom, "first-meta")
  expect_equal(h2$Metadata$custom, "second-meta")
  unlink(tmp)
  aws_s3_download(file = tmp, key = "x", bucket = bucket, version = v1)
  expect_equal(readLines(tmp), "first")
  aws_s3_download(file = tmp, key = "x", bucket = bucket, version = v2)
  expect_equal(readLines(tmp), "second")
})

tar_test("graceful error on multipart upload", {
  bucket <- random_bucket_name()
  paws.storage::s3()$create_bucket(Bucket = bucket)
  on.exit(aws_s3_delete_bucket(bucket))
  tmp <- tempfile()
  writeBin(raw(1e4), tmp)
  expect_error(
    aws_s3_upload(
      file = tmp,
      key = "x",
      bucket = bucket,
      multipart = TRUE,
      part_size = 5e3
    ),
    class = "tar_condition_file"
  )
})
