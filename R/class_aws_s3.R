#' @export
store_read_object.tar_aws_s3 <- function(store) {
  path <- store$file$path
  bucket <- store_aws_s3_bucket(path)
  key <- store_aws_s3_key(path)
  tmp <- tempfile()
  aws.s3::save_object(object = key, bucket = bucket, file = tmp)
  store_coerce_object(store, store_read_path(store, tmp))
}

#' @export
store_write_object.tar_aws_s3 <- function(store, object) {
  tmp <- tempfile()
  dir_create(dirname(tmp))
  store_write_path(store, store_coerce_object(store, object), tmp)
  store$file$hash <- file_hash(tmp)
  key <- store_aws_s3_key(store$file$path)
  bucket <- store_aws_s3_bucket(store$file$path)
  aws.s3::put_object(
    file = tmp,
    object = key,
    bucket = bucket,
    multipart = TRUE,
    headers = c("x-amz-meta-targets-hash" = store$file$hash)
  )
  store_aws_s3_wait_upload(key, bucket, store$file$hash)
}

#' @export
store_produce_path.tar_aws_s3 <- function(store, name, object) {
  bucket <- store$resources$bucket
  assert_chr(bucket, "invalid S3 bucket name.")
  assert_scalar(bucket, "invalid S3 bucket name.")
  assert_nzchar(bucket, "invalid S3 bucket name.")
  prefix <- store$resources$prefix %||% path_default_dir()
  assert_chr(prefix, "invalid S3 prefix.")
  assert_scalar(prefix, "invalid S3 prefix.")
  object <- file.path(prefix, name)
  assert_nzchar(object, "invalid S3 object key.")
  c(bucket, object)
}

store_aws_s3_bucket <- function(path) {
  path[1]
}

store_aws_s3_key <- function(path) {
  path[2]
}

store_aws_s3_wait_upload <- function(
  key,
  bucket,
  hash,
  sleep = 0.01,
  timeout = 300
) {
  time_left <- timeout
  while (time_left > 0) {
    if (store_aws_s3_is_uploaded(key, bucket, hash)) {
      return(invisible())
    }
    Sys.sleep(sleep)
    time_left <- time_left - sleep
  }
  throw_file("timed out waiting for ", key, " S3 object upload.")
}

store_aws_s3_is_uploaded <- function(key, bucket, hash_local) {
  identical(hash_local, store_aws_s3_hash(key = key, bucket = bucket))
}

store_aws_s3_hash <- function(key, bucket) {
  head <- aws.s3::head_object(object = key, bucket = bucket)
  hash_remote <- attr(head, "x-amz-meta-targets-hash")
}

#' @export
store_has_correct_hash.tar_aws_s3 <- function(store, file) {
  path <- file$path
  bucket <- store_aws_s3_bucket(path)
  key <- store_aws_s3_key(path)
  hash <- tryCatch(store_aws_s3_hash, error = function(e) NA_character_)
  identical(hash, file$hash)
}

#' @export
store_late_hash.tar_aws_s3 <- function(store) {
}

#' @export
store_wait_correct_hash.tar_aws_s3 <- function(store, remote) {
}

#' @export
store_validate_packages.tar_aws_s3 <- function(store) {
  assert_package("aws.s3")
  NextMethod()
}
