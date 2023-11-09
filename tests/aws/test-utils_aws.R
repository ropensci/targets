# Use sparingly to minimize AWS costs.
# Verify all `targets` buckets are deleted afterwards.
tar_test("aws_s3_exists()", {
  bucket <- random_bucket_name()
  paws.storage::s3()$create_bucket(Bucket = bucket)
  on.exit(aws_s3_delete_bucket(bucket))
  expect_false(
    aws_s3_exists(
      key = "x",
      bucket = bucket,
      max_tries = 1L
    )
  )
  tmp <- tempfile()
  writeLines("x", tmp)
  paws.storage::s3()$put_object(
    Body = tmp,
    Key = "x",
    Bucket = bucket
  )
  expect_true(
    aws_s3_exists(
      key = "x",
      bucket = bucket,
      max_tries = 1L
    )
  )
  expect_false(
    aws_s3_exists(
      key = "x",
      bucket = bucket,
      args = list(ExpectedBucketOwner = "phantom_f4acd87c52d4e62b"),
      max_tries = 1L
    )
  )
})

tar_test("aws_s3_head()", {
  bucket <- random_bucket_name()
  paws.storage::s3()$create_bucket(Bucket = bucket)
  on.exit(aws_s3_delete_bucket(bucket))
  expect_null(
    aws_s3_head(
      key = "x",
      bucket = random_bucket_name(),
      max_tries = 1L
    )
  )
  expect_null(
    aws_s3_head(
      key = "x",
      bucket = bucket,
      max_tries = 1L
    )
  )
  tmp <- tempfile()
  writeLines("x", tmp)
  paws.storage::s3()$put_object(Body = tmp, Key = "x", Bucket = bucket)
  head <- aws_s3_head(
    key = "x",
    bucket = bucket,
    max_tries = 1L
  )
  expect_true(is.list(head))
  expect_true(is.character(head$ETag))
  expect_true(nzchar(head$ETag))
  expect_null(
    aws_s3_head(
      key = "x",
      bucket = bucket,
      args = list(ExpectedBucketOwner = "phantom_f4acd87c52d4e62b"),
      max_tries = 1L
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
  aws_s3_download(
    file = tmp2,
    key = "x",
    bucket = bucket,
    max_tries = 1L
  )
  expect_error(
    aws_s3_download(
      file = tmp2,
      key = "x",
      bucket = bucket,
      args = list(ExpectedBucketOwner = "phantom_f4acd87c52d4e62b"),
      max_tries = 1L
    ),
    class = "http_400"
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
  expect_true(
    aws_s3_exists(
      key = key,
      bucket = bucket,
      max_tries = 1L
    )
  )
  expect_error(
    aws_s3_delete(
      key = key,
      bucket = bucket,
      args = list(ExpectedBucketOwner = "phantom_f4acd87c52d4e62b"),
      max_tries = 1L
    ),
    class = "http_400"
  )
  expect_true(
    aws_s3_exists(
      key = key,
      bucket = bucket,
      max_tries = 1L
    )
  )
  aws_s3_delete(
    key = key,
    bucket = bucket,
    max_tries = 1L
  )
  expect_false(
    aws_s3_exists(
      key = key,
      bucket = bucket,
      max_tries = 1L
    )
  )
  aws_s3_delete(
    key = key,
    bucket = bucket,
    max_tries = 1L
  )
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
  expect_true(
    aws_s3_exists(
      key = key,
      bucket = bucket,
      version = version1,
      max_tries = 1L
    )
  )
  expect_true(
    aws_s3_exists(
      key = key,
      bucket = bucket,
      version = version2,
      max_tries = 1L
    )
  )
  aws_s3_delete(
    key = key,
    bucket = bucket,
    version = version1,
    max_tries = 1L
  )
  expect_false(
    aws_s3_exists(
      key = key,
      bucket = bucket,
      version = version1,
      max_tries = 1L
    )
  )
  expect_true(
    aws_s3_exists(
      key = key,
      bucket = bucket,
      version = version2,
      max_tries = 1L
    )
  )
})

tar_test("aws_s3_delete_objects()", {
  bucket <- random_bucket_name()
  client <- paws.storage::s3()
  client$create_bucket(Bucket = bucket)
  on.exit(aws_s3_delete_bucket(bucket))
  for (key in letters) {
    client$put_object(
      Bucket = bucket,
      Key = key,
      Body = charToRaw("contents")
    )
  }
  for (key in letters) {
    expect_true(aws_s3_exists(key = key, bucket = bucket))
  }
  objects <- lapply(letters, function(x) list(Key = x))
  expect_error(
    aws_s3_delete_objects(
      objects = objects,
      bucket = bucket,
      args = list(ExpectedBucketOwner = "phantom_f4acd87c52d4e62b"),
      max_tries = 1L
    ),
    class = "http_400"
  )
  for (key in letters) {
    expect_true(aws_s3_exists(key = key, bucket = bucket))
  }
  aws_s3_delete_objects(
    objects = objects,
    bucket = bucket,
    batch_size = 10L
  )
  for (key in letters) {
    expect_false(aws_s3_exists(key = key, bucket = bucket))
  }
})

tar_test("aws_s3_delete_objects() versioned", {
  bucket <- random_bucket_name()
  client <- paws.storage::s3()
  client$create_bucket(Bucket = bucket)
  on.exit(aws_s3_delete_bucket(bucket))
  client$put_bucket_versioning(
    Bucket = bucket,
    VersioningConfiguration = list(
      Status = "Enabled"
    )
  )
  objects <- list()
  index <- 1L
  for (key in letters) {
    head <- client$put_object(
      Bucket = bucket,
      Key = key,
      Body = charToRaw("contents")
    )
    objects[[index]] <- list(Key = key, VersionId = head$VersionId)
    index <- index + 1L
  }
  for (key in letters) {
    client$put_object(
      Bucket = bucket,
      Key = key,
      Body = charToRaw("contents_version2")
    )
  }
  for (index in seq_along(letters)) {
    expect_true(
      aws_s3_exists(
        key = objects[[index]]$Key,
        bucket = bucket,
        version = objects[[index]]$VersionId
      )
    )
  }
  expect_error(
    aws_s3_delete_objects(
      objects = objects,
      bucket = bucket,
      args = list(ExpectedBucketOwner = "phantom_f4acd87c52d4e62b"),
      max_tries = 1L
    ),
    class = "http_400"
  )
  for (index in seq_along(letters)) {
    expect_true(
      aws_s3_exists(
        key = objects[[index]]$Key,
        bucket = bucket,
        version = objects[[index]]$VersionId
      )
    )
  }
  aws_s3_delete_objects(
    objects = objects,
    bucket = bucket,
    batch_size = 10L
  )
  for (index in seq_along(letters)) {
    expect_false(
      aws_s3_exists(
        key = objects[[index]]$Key,
        bucket = bucket,
        version = objects[[index]]$VersionId
      )
    )
    expect_true(
      aws_s3_exists(
        key = objects[[index]]$Key,
        bucket = bucket
      )
    )
  }
})

tar_test("aws_s3_upload() without headers", {
  bucket <- random_bucket_name()
  paws.storage::s3()$create_bucket(Bucket = bucket)
  on.exit(aws_s3_delete_bucket(bucket))
  expect_false(
    aws_s3_exists(
      key = "x",
      bucket = bucket,
      max_tries = 1L
    )
  )
  tmp <- tempfile()
  writeLines("x", tmp)
  expect_error(
    aws_s3_upload(
      file = tmp,
      key = "x",
      bucket = bucket,
      args = list(ExpectedBucketOwner = "phantom_f4acd87c52d4e62b"),
      max_tries = 1L
    ),
    class = "http_400"
  )
  expect_false(
    aws_s3_exists(
      key = "x",
      bucket = bucket,
      max_tries = 1L
    )
  )
  aws_s3_upload(
    file = tmp,
    key = "x",
    bucket = bucket,
    max_tries = 1L
  )
  expect_true(
    aws_s3_exists(
      key = "x",
      bucket = bucket,
      max_tries = 1L
    )
  )
})

tar_test("aws_s3_upload() and download with metadata and region", {
  bucket <- random_bucket_name()
  region <- ifelse(
    Sys.getenv("AWS_REGION") == "us-east-1",
    "us-east-2",
    "us-east-1"
  )
  client <- paws.storage::s3(config = list(region = region))
  client$create_bucket(
    Bucket = bucket,
    CreateBucketConfiguration = list(LocationConstraint = region)
  )
  on.exit(aws_s3_delete_bucket(bucket, client = client))
  expect_false(
    aws_s3_exists(
      key = "x",
      bucket = bucket,
      region = region,
      max_tries = 1L
    )
  )
  tmp <- tempfile()
  writeLines("x", tmp)
  aws_s3_upload(
    file = tmp,
    key = "x",
    bucket = bucket,
    metadata = list("custom" = "custom_metadata"),
    region = region,
    max_tries = 1L
  )
  expect_true(
    aws_s3_exists(
      key = "x",
      bucket = bucket,
      region = region,
      max_tries = 1L
    )
  )
  head <- aws_s3_head(
    key = "x",
    bucket = bucket,
    region = region,
    max_tries = 1L
  )
  expect_equal(head$Metadata$custom, "custom_metadata")
  tmp2 <- tempfile()
  expect_false(file.exists(tmp2))
  aws_s3_download(
    file = tmp2,
    key = "x",
    bucket = bucket,
    region = region,
    max_tries = 1L
  )
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
    metadata = list("custom" = "first-meta"),
    max_tries = 1L
  )
  v1 <- head_first$VersionId
  writeLines("second", tmp)
  head_second <- aws_s3_upload(
    file = tmp,
    key = "x",
    bucket = bucket,
    metadata = list("custom" = "second-meta"),
    max_tries = 1L
  )
  v2 <- head_second$VersionId
  expect_true(
    aws_s3_exists(
      key = "x",
      bucket = bucket,
      max_tries = 1L
    )
  )
  expect_true(
    aws_s3_exists(
      key = "x",
      bucket = bucket,
      version = v1,
      max_tries = 1L
    )
  )
  expect_true(
    aws_s3_exists(
      key = "x",
      bucket = bucket,
      version = v2,
      max_tries = 1L
    )
  )
  expect_false(
    aws_s3_exists(
      key = "x",
      bucket = bucket,
      version = "v3",
      max_tries = 1L
    )
  )
  h1 <- aws_s3_head(
    key = "x",
    bucket = bucket,
    version = v1,
    max_tries = 1L
  )
  h2 <- aws_s3_head(
    key = "x",
    bucket = bucket,
    version = v2,
    max_tries = 1L
  )
  expect_equal(h1$VersionId, v1)
  expect_equal(h2$VersionId, v2)
  expect_equal(h1$Metadata$custom, "first-meta")
  expect_equal(h2$Metadata$custom, "second-meta")
  unlink(tmp)
  aws_s3_download(
    file = tmp,
    key = "x",
    bucket = bucket,
    version = v1,
    max_tries = 1L
  )
  expect_equal(readLines(tmp), "first")
  aws_s3_download(
    file = tmp,
    key = "x",
    bucket = bucket,
    version = v2,
    max_tries = 1L
  )
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
    metadata = list("custom" = "first-meta"),
    max_tries = 1L
  )
  v1 <- head_first$VersionId
  writeLines("second", tmp)
  head_second <- aws_s3_upload(
    file = tmp,
    key = "x",
    bucket = bucket,
    multipart = TRUE,
    metadata = list("custom" = "second-meta"),
    max_tries = 1L
  )
  v2 <- head_second$VersionId
  expect_true(
    aws_s3_exists(
      key = "x",
      bucket = bucket,
      max_tries = 1L
    )
  )
  expect_true(
    aws_s3_exists(
      key = "x",
      bucket = bucket,
      version = v1,
      max_tries = 1L
    )
  )
  expect_true(
    aws_s3_exists(
      key = "x",
      bucket = bucket,
      version = v2,
      max_tries = 1L
    )
  )
  expect_false(
    aws_s3_exists(
      key = "x",
      bucket = bucket,
      version = "v3",
      max_tries = 1L
    )
  )
  h1 <- aws_s3_head(
    key = "x",
    bucket = bucket,
    version = v1,
    max_tries = 1L
  )
  h2 <- aws_s3_head(
    key = "x",
    bucket = bucket,
    version = v2,
    max_tries = 1L
  )
  expect_equal(h1$VersionId, v1)
  expect_equal(h2$VersionId, v2)
  expect_equal(h1$Metadata$custom, "first-meta")
  expect_equal(h2$Metadata$custom, "second-meta")
  unlink(tmp)
  aws_s3_download(
    file = tmp,
    key = "x",
    bucket = bucket,
    version = v1,
    max_tries = 1L
  )
  expect_equal(readLines(tmp), "first")
  aws_s3_download(
    file = tmp,
    key = "x",
    bucket = bucket,
    version = v2,
    max_tries = 1L
  )
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
      part_size = 5e3,
      max_tries = 1L
    ),
    class = "tar_condition_file"
  )
})

tar_test("aws_s3_list_etags()", {
  bucket <- random_bucket_name()
  paws.storage::s3()$create_bucket(Bucket = bucket)
  on.exit(aws_s3_delete_bucket(bucket))
  expect_equal(
    aws_s3_list_etags(prefix = "/", bucket = bucket),
    list()
  )
  for (key in c("w", "x", "y", "z")) {
    paws.storage::s3()$put_object(
      Body = charToRaw(key),
      Key = key,
      Bucket = bucket
    )
  }
  out <- aws_s3_list_etags(prefix = "", bucket = bucket)
  out2 <- aws_s3_list_etags(prefix = "", bucket = bucket, page_size = 2L)
  expect_equal(out, out2)
  expect_equal(length(out), 4L)
  expect_equal(sort(names(out)), sort(c("w", "x", "y", "z")))
  for (etag in out) {
    expect_true(is.character(etag))
    expect_true(!anyNA(etag))
    expect_equal(length(etag), 1L)
    expect_gt(nchar(etag), 10L)
  }
})
