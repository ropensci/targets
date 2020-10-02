# Semi-automated tests of Amazon S3 integration live in tests/aws/. # nolint
# These tests should not be fully automated because they
# automatically create S3 buckets and upload data,
# which could put an unexpected and unfair burden on
# external contributors from the open source community.
# nocov start
#' @export
store_produce_path.tar_aws_s3 <- function(store, name, object) {
  bucket <- store$resources$bucket
  assert_nonempty(bucket, "S3 bucket name cannot be empty.")
  assert_chr(bucket, "S3 bucket name must be character.")
  assert_scalar(bucket, "invalid S3 bucket name.")
  assert_nzchar(bucket, "invalid S3 bucket name.")
  prefix <- store$resources$prefix %||% path_default_dir()
  assert_nonempty(prefix, "S3 object prefix cannot be empty.")
  assert_chr(prefix, "invalid S3 prefix.")
  assert_scalar(prefix, "invalid S3 prefix.")
  object <- file.path(prefix, name)
  assert_nzchar(object, "invalid S3 object key.")
  c(bucket, object)
}

#' @export
store_produce_stage.tar_aws_s3 <- function(store, name, object) {
  tempfile(pattern = name)
}

store_aws_s3_bucket <- function(path) {
  path[1]
}

store_aws_s3_key <- function(path) {
  path[2]
}

#' @export
store_read_object.tar_aws_s3 <- function(store) {
  path <- store$file$path
  bucket <- store_aws_s3_bucket(path)
  key <- store_aws_s3_key(path)
  tmp <- tempfile()
  on.exit(unlink(tmp))
  aws.s3::save_object(object = key, bucket = bucket, file = tmp)
  store_coerce_object(store, store_read_path(store, tmp))
}

#' @export
store_write_object.tar_aws_s3 <- function(store, object) {
  stage <- store$file$stage
  dir_create(dirname(stage))
  store_write_path(store, store_coerce_object(store, object), stage)
}

#' @export
store_upload_object.tar_aws_s3 <- function(store) {
  key <- store_aws_s3_key(store$file$path)
  bucket <- store_aws_s3_bucket(store$file$path)
  hash <- store$file$hash
  aws.s3::put_object(
    file = store$file$stage,
    object = key,
    bucket = bucket,
    multipart = TRUE,
    headers = c("x-amz-meta-targets-hash" = hash)
  )
}

store_aws_s3_exists <- function(key, bucket) {
  suppressWarnings(aws.s3::object_exists(object = key, bucket = bucket))
}

store_aws_s3_hash <- function(key, bucket) {
  head <- aws.s3::head_object(object = key, bucket = bucket)
  hash_remote <- attr(head, "x-amz-meta-targets-hash")
}

#' @export
store_late_hash.tar_aws_s3 <- function(store) {
  file <- file_init(path = store$file$stage)
  file_update_hash(file)
  store$file$hash <- file$hash
  store$file$bytes <- file$bytes
  store$file$time <- file$time
}

#' @export
store_has_correct_hash.tar_aws_s3 <- function(store) {
  bucket <- store_aws_s3_bucket(store$file$path)
  key <- store_aws_s3_key(store$file$path)
  trn(
    store_aws_s3_exists(key, bucket),
    identical(store_aws_s3_hash(key, bucket), store$file$hash),
    FALSE
  )
}

#' @export
store_ensure_correct_hash.tar_aws_s3 <- function(store, storage, deployment) {
  store_wait_correct_hash(store)
}
# nocov end

#' @export
store_validate_packages.tar_aws_s3 <- function(store) {
  assert_package("aws.s3")
  NextMethod()
}
