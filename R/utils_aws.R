# Semi-automated tests of Amazon S3 integration live in tests/aws/. # nolint
# These tests should not be fully automated because they
# automatically create S3 buckets and upload data,
# which could put an unexpected and unfair burden on
# external contributors from the open source community.
# nocov start
aws_exists <- function(key, bucket, region = NULL) {
  suppressWarnings(
    aws.s3::object_exists(
      object = key,
      bucket = bucket,
      region = region,
      check_region = is.null(region)
    )
  )
}

aws_head <- function(key, bucket, region = NULL) {
  head <- aws.s3::head_object(
    object = key,
    bucket = bucket,
    region = region,
    check_region = is.null(region)
  )
}

aws_download <- function(
  file,
  key,
  bucket,
  region = NULL
) {
  aws.s3::save_object(
    object = key,
    bucket = bucket,
    file = file,
    region = region,
    check_region = is.null(region)
  )
}

aws_upload <- function(
  file,
  key,
  bucket,
  region = NULL,
  headers = character(0)
) {
  aws.s3::put_object(
    file = file,
    object = key,
    bucket = bucket,
    multipart = TRUE,
    headers = headers,
    region = region,
    check_region = is.null(region)
  )
}

aws_version <- function(key, bucket, region = NULL) {
  head <- aws_head(
    key = key,
    bucket = bucket,
    region = region
  )
  attr(head, "x-amz-version-id")
}
# nocov end
