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
  prefix <- store$resources$aws$prefix %|||%
    store$resources$prefix %|||%
    path_objects_dir_cloud()
  tar_assert_nonempty(prefix)
  tar_assert_chr(prefix)
  tar_assert_scalar(prefix)
  key <- file.path(prefix, name)
  tar_assert_nzchar(key)
  bucket <- paste0("bucket=", bucket)
  region <- paste0("region=", if_any(is.null(region), "NULL", region))
  key <- paste0("key=", key)
  c(bucket, region, key)
}

store_aws_bucket <- function(path) {
  # compatibility with targets <= 0.8.1:
  if (store_aws_path_0.8.1(path)) {
    return(path[1L])
  }
  # with metadata written by targets > 0.8.1:
  store_aws_path_field(path = path, pattern = "^bucket=")
}

store_aws_region <- function(path) {
  # compatibility with targets <= 0.8.1:
  if (store_aws_path_0.8.1(path)) {
    return()
  }
  # with metadata written by targets > 0.8.1:
  out <- store_aws_path_field(path = path, pattern = "^region=")
  out <- if_any(length(out) > 0L && any(nzchar(out)), out, "")
  if_any(identical(out, "NULL"), NULL, out)
}

store_aws_key <- function(path) {
  # compatibility with targets <= 0.8.1:
  if (store_aws_path_0.8.1(path)) {
    return(path[2L])
  }
  store_aws_path_field(path = path, pattern = "^key=")
}

store_aws_path_field <- function(path, pattern) {
  path <- store_aws_split_colon(path)
  keyvalue_field(x = path, pattern = pattern)
}

store_aws_version <- function(path) {
  out <- store_aws_path_field(path = path, pattern = "^version=")
  if_any(length(out) && nzchar(out), out, NULL)
}

store_aws_path_0.8.1 <- function(path) {
  !any(grepl(pattern = "^bucket=", x = path))
}

# Tech debt from a dev version. Need to be compatible.
store_aws_split_colon <- function(path) {
  index <- grep(pattern = "^bucket=", x = path)
  bucket_pair <- unlist(strsplit(x = path[index], split = ":"))
  c(bucket_pair, path[-index])
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
  tmp <- tempfile()
  on.exit(unlink(tmp))
  aws_s3_download(
    key = store_aws_key(path),
    bucket = store_aws_bucket(path),
    file = tmp,
    region = store_aws_region(path),
    version = store_aws_version(path)
  )
  store_convert_object(store, store_read_path(store, tmp))
}

#' @export
store_upload_object.tar_aws <- function(store) {
  key <- store_aws_key(store$file$path)
  head <- if_any(
    file_exists_stage(store$file),
    aws_s3_upload(
      file = store$file$stage,
      key = key,
      bucket = store_aws_bucket(store$file$path),
      region = store_aws_region(store$file$path),
      metadata = list("targets-hash" = store$file$hash),
      part_size = store$resources$aws$part_size %|||% (5 * (2 ^ 20))
    ),
    tar_throw_file(
      "Cannot upload non-existent AWS staging file ",
      store$file$stage,
      " to key ",
      key,
      ". The target probably encountered an error."
    )
  )
  path <- grep(
    pattern = "^version=",
    x = store$file$path,
    value = TRUE,
    invert = TRUE
  )
  store$file$path <- c(path, paste0("version=", head$VersionId))
  invisible()
}

#' @export
store_has_correct_hash.tar_aws <- function(store) {
  path <- store$file$path
  bucket <- store_aws_bucket(path)
  region <- store_aws_region(path)
  key <- store_aws_key(path)
  version <- store_aws_version(path)
  if_any(
    aws_s3_exists(
      key = key,
      bucket = bucket,
      region = region,
      version = version
    ),
    identical(
      store_aws_hash(
        key = key,
        bucket = bucket,
        region = region,
        version = version
      ),
      store$file$hash
    ),
    FALSE
  )
}

store_aws_hash <- function(key, bucket, region, version) {
  head <- aws_s3_head(
    key = key,
    bucket = bucket,
    region = region,
    version = version
  )
  head$Metadata[["targets-hash"]]
}
# nocov end

#' @export
store_get_packages.tar_aws <- function(store) {
  c("paws", NextMethod())
}
