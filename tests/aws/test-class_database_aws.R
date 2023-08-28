# Use sparingly to minimize AWS costs.
# Verify all `targets` buckets are deleted afterwards.
tar_test("aws database basic methods", {
  skip_if_no_aws()
  s3 <- paws.storage::s3()
  bucket <- random_bucket_name()
  s3$create_bucket(Bucket = bucket)
  on.exit(aws_s3_delete_bucket(bucket))
  x <- database_init(
    path = tempfile(),
    subkey = "meta/key",
    resources = tar_resources(
      aws = tar_resources_aws(bucket = bucket, prefix = "custom/prefix")
    ),
    repository = "aws"
  )
  key <- "custom/prefix/meta/key"
  writeLines("meta_lines", x$path)
  expect_false(
    aws_s3_exists(
      key = key,
      bucket = bucket,
      max_tries = 20
    )
  )
  x$upload()
  expect_true(
    aws_s3_exists(
      key = key,
      bucket = bucket,
      max_tries = 20
    )
  )
  head <- x$head()
  file <- file_init(x$path)
  file_ensure_hash(file)
  expect_true(head$exists)
  expect_equal(head$hash, file$hash)
  expect_equal(head$size, file$size)
  expect_equal(head$time, file$time)
  x$path <- tempfile()
  expect_false(file.exists(x$path))
  x$download()
  expect_true(file.exists(x$path))
  expect_equal(readLines(x$path), "meta_lines")
  file <- file_init(x$path)
  file_ensure_hash(file)
  expect_equal(head$hash, file$hash)
  expect_equal(head$size, file$size)
  expect_true(
    aws_s3_exists(
      key = key,
      bucket = bucket,
      max_tries = 20
    )
  )
  x$delete_cloud()
  expect_false(
    aws_s3_exists(
      key = key,
      bucket = bucket,
      max_tries = 20
    )
  )
})

tar_test("aws database sync upload", {
  skip_if_no_aws()
  s3 <- paws.storage::s3()
  bucket <- random_bucket_name()
  s3$create_bucket(Bucket = bucket)
  on.exit(aws_s3_delete_bucket(bucket))
  x <- database_init(
    path = tempfile(),
    subkey = "meta/key",
    resources = tar_resources(
      aws = tar_resources_aws(bucket = bucket, prefix = "custom/prefix")
    ),
    repository = "aws"
  )
  key <- "custom/prefix/meta/key"
  writeLines("meta_lines", x$path)
  expect_false(
    aws_s3_exists(
      key = key,
      bucket = bucket,
      max_tries = 20
    )
  )
  x$sync()
  expect_true(
    aws_s3_exists(
      key = key,
      bucket = bucket,
      max_tries = 20
    )
  )
  Sys.sleep(2)
  writeLines("meta_lines2", x$path)
  x$sync()
  x$path <- tempfile()
  x$download()
  expect_equal(readLines(x$path), "meta_lines2")
})

tar_test("aws database sync download", {
  skip_if_no_aws()
  s3 <- paws.storage::s3()
  bucket <- random_bucket_name()
  s3$create_bucket(Bucket = bucket)
  on.exit(aws_s3_delete_bucket(bucket))
  x <- database_init(
    path = tempfile(),
    subkey = "meta/key",
    resources = tar_resources(
      aws = tar_resources_aws(bucket = bucket, prefix = "custom/prefix")
    ),
    repository = "aws"
  )
  key <- "custom/prefix/meta/key"
  writeLines("meta_lines", x$path)
  x$upload()
  x$path <- tempfile()
  expect_false(file.exists(x$path))
  x$sync()
  expect_true(file.exists(x$path))
  expect_equal(readLines(x$path), "meta_lines")
  Sys.sleep(2)
  writeLines("meta_lines2", x$path)
  x$sync()
  x$path <- tempfile()
  x$download()
  expect_equal(readLines(x$path), "meta_lines2")
})
