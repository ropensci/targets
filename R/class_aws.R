#' @export
store_produce_path.tar_aws <- function(store, name, object, path_store) {
  store_produce_aws_path(
    store = store,
    name = name,
    object = object,
    path_store = path_store
  )
}

store_produce_aws_path <- function(store, name, object, path_store) {
  bucket <- store$resources$aws$bucket %|||% store$resources$bucket
  tar_assert_nonempty(bucket)
  tar_assert_chr(bucket)
  tar_assert_scalar(bucket)
  tar_assert_nzchar(bucket)
  prefix <- store$resources$aws$prefix %|||%
    store$resources$prefix %|||%
    path_objects_dir_cloud()
  tar_assert_nonempty(prefix)
  tar_assert_chr(prefix)
  tar_assert_scalar(prefix)
  object <- file.path(prefix, name)
  tar_assert_nzchar(object)
  c(bucket, object)
}

# Semi-automated tests of Amazon S3 integration live in tests/aws/. # nolint
# These tests should not be fully automated because they
# automatically create S3 buckets and upload data,
# which could put an unexpected and unfair burden on
# external contributors from the open source community.
# nocov start
store_aws_bucket <- function(path) {
  path[1L]
}

store_aws_key <- function(path) {
  path[2L]
}

store_aws_path <- function(path) {
  path[-seq_len(2L)]
}

#' @export
store_read_object.tar_aws <- function(store) {
  path <- store$file$path
  bucket <- store_aws_bucket(path)
  key <- store_aws_key(path)
  tmp <- tempfile()
  on.exit(unlink(tmp))
  aws.s3::save_object(
    object = key,
    bucket = bucket,
    file = tmp,
    check_region = TRUE
  )
  store_cast_object(store, store_read_path(store, tmp))
}

#' @export
store_upload_object.tar_aws <- function(store) {
  key <- store_aws_key(store$file$path)
  bucket <- store_aws_bucket(store$file$path)
  hash <- store$file$hash
  if_any(
    file_exists_stage(store$file),
    aws.s3::put_object(
      file = store$file$stage,
      object = key,
      bucket = bucket,
      multipart = TRUE,
      headers = c("x-amz-meta-targets-hash" = hash),
      check_region = TRUE
    ),
    tar_throw_file(
      "Cannot upload non-existent AWS staging file ",
      store$file$stage,
      " to key ",
      key,
      ". The target probably encountered an error."
    )
  )
}

store_aws_exists <- function(key, bucket) {
  suppressWarnings(
    aws.s3::object_exists(
      object = key,
      bucket = bucket,
      check_region = TRUE
    )
  )
}

store_aws_hash <- function(key, bucket) {
  head <- aws.s3::head_object(
    object = key,
    bucket = bucket,
    check_region = TRUE
  )
  hash_worker <- attr(head, "x-amz-meta-targets-hash")
}

#' @export
store_has_correct_hash.tar_aws <- function(store) {
  bucket <- store_aws_bucket(store$file$path)
  key <- store_aws_key(store$file$path)
  if_any(
    store_aws_exists(key, bucket),
    identical(store_aws_hash(key, bucket), store$file$hash),
    FALSE
  )
}
# nocov end

#' @export
store_get_packages.tar_aws <- function(store) {
  c("aws.s3", NextMethod())
}
