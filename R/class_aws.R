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
  region <- store$resources$aws$region %|||% store$resources$region
  tar_assert_nonempty(bucket)
  tar_assert_chr(bucket)
  tar_assert_scalar(bucket)
  tar_assert_nzchar(bucket)
  tar_assert_nonempty(region %|||% "region")
  tar_assert_chr(region %|||% "region")
  tar_assert_scalar(region %|||% "region")
  tar_assert_nzchar(region %|||% "region")
  prefix <- store$resources$aws$prefix %|||%
    store$resources$prefix %|||%
    path_objects_dir_cloud()
  tar_assert_nonempty(prefix)
  tar_assert_chr(prefix)
  tar_assert_scalar(prefix)
  metabucket <- store_produce_aws_metabucket(bucket = bucket, region = region)
  object <- file.path(prefix, name)
  tar_assert_nzchar(object)
  c(metabucket, object)
}

store_produce_aws_metabucket <- function(bucket, region) {
  bucket <- paste0("bucket=", bucket)
  region <- paste0("region=", region)
  paste(c(bucket, region), collapse = ":")
}

store_aws_bucket <- function(path) {
  pattern <- "^bucket="
  # compatibility with targets <= 0.8.1:
  if (!any(grepl(pattern = pattern, x = path[1L]))) {
    return(path[1L])
  }
  # with metadata written by targets > 0.8.1:
  metabucket <- unlist(strsplit(x = path[1L], split = ":"))
  bucket <- grep(pattern = pattern, x = metabucket, value = TRUE)
  gsub(pattern = pattern, replacement = "", x = bucket)
}

store_aws_region <- function(path) {
  pattern <- "^region="
  metabucket <- unlist(strsplit(x = path[1L], split = ":"))
  region <- grep(pattern = pattern, x = metabucket, value = TRUE)
  out <- gsub(pattern = pattern, replacement = "", x = region)
  if_any(length(out) > 0L && any(nzchar(out)), out, NULL)
}

store_aws_key <- function(path) {
  path[2L]
}

store_aws_path <- function(path) {
  path[-seq_len(2L)]
}

# Semi-automated tests of Amazon S3 integration live in tests/aws/. # nolint
# These tests should not be fully automated because they
# automatically create S3 buckets and upload data,
# which could put an unexpected and unfair burden on
# external contributors from the open source community.
# nocov start
#' @export
store_read_object.tar_aws <- function(store) {
  path <- store$file$path
  bucket <- store_aws_bucket(path)
  region <- store_aws_region(path)
  key <- store_aws_key(path)
  tmp <- tempfile()
  on.exit(unlink(tmp))
  aws.s3::save_object(
    object = key,
    bucket = bucket,
    file = tmp,
    region = region,
    check_region = TRUE
  )
  store_cast_object(store, store_read_path(store, tmp))
}

#' @export
store_upload_object.tar_aws <- function(store) {
  key <- store_aws_key(store$file$path)
  bucket <- store_aws_bucket(store$file$path)
  region <- store_aws_region(store$file$path)
  hash <- store$file$hash
  if_any(
    file_exists_stage(store$file),
    aws.s3::put_object(
      file = store$file$stage,
      object = key,
      bucket = bucket,
      multipart = TRUE,
      headers = c("x-amz-meta-targets-hash" = hash),
      region = region,
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

store_aws_exists <- function(key, bucket, region) {
  suppressWarnings(
    aws.s3::object_exists(
      object = key,
      bucket = bucket,
      region = region,
      check_region = TRUE
    )
  )
}

store_aws_hash <- function(key, bucket, region) {
  head <- aws.s3::head_object(
    object = key,
    bucket = bucket,
    region = region,
    check_region = TRUE
  )
  hash_worker <- attr(head, "x-amz-meta-targets-hash")
}

#' @export
store_has_correct_hash.tar_aws <- function(store) {
  path <- store$file$path
  bucket <- store_aws_bucket(path)
  region <- store_aws_region(path)
  key <- store_aws_key(path)
  if_any(
    store_aws_exists(key, bucket, region),
    identical(store_aws_hash(key, bucket, region), store$file$hash),
    FALSE
  )
}
# nocov end

#' @export
store_get_packages.tar_aws <- function(store) {
  c("aws.s3", NextMethod())
}
