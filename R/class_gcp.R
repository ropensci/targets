#' @export
store_produce_path.tar_gcp <- function(store, name, object, path_store) {
  store_produce_gcp_path(
    store = store,
    name = name,
    object = object,
    path_store = path_store
  )
}

#' @export
store_class_repository.gcp <- function(repository, store, format) {
  format <- gsub(pattern = "\\&.*$", replacement = "", x = format)
  c(
    sprintf("tar_gcp_%s", format),
    "tar_gcp",
    "tar_cloud",
    if_any("tar_external" %in% class(store), character(0), "tar_external"),
    class(store)
  )
}

#' @export
store_assert_repository_setting.gcp <- function(repository) {
}

store_produce_gcp_path <- function(store, name, object, path_store) {
  bucket <- store$resources$gcp$bucket %|||% store$resources$bucket
  region <- store$resources$gcp$region %|||% store$resources$region
  endpoint <- store$resources$gcp$endpoint %|||% store$resources$endpoint
  tar_assert_nonempty(bucket)
  tar_assert_chr(bucket)
  tar_assert_scalar(bucket)
  tar_assert_nzchar(bucket)
  tar_assert_nonempty(region %|||% "region")
  tar_assert_chr(region %|||% "region")
  tar_assert_scalar(region %|||% "region")
  tar_assert_nonempty(endpoint %|||% "endpoint")
  tar_assert_chr(endpoint %|||% "endpoint")
  tar_assert_scalar(endpoint %|||% "endpoint")
  prefix <- store$resources$gcp$prefix %|||%
    store$resources$prefix %|||%
    path_objects_dir_cloud()
  tar_assert_nonempty(prefix)
  tar_assert_chr(prefix)
  tar_assert_scalar(prefix)
  key <- file.path(prefix, name)
  tar_assert_nzchar(key)
  bucket <- paste0("bucket=", bucket)
  region <- paste0("region=", if_any(is.null(region), "NULL", region))
  endpoint <- if_any(is.null(endpoint), "NULL", endpoint)
  endpoint <- base64url::base64_urlencode(endpoint)
  endpoint <- paste0("endpoint=", endpoint)
  key <- paste0("key=", key)
  c(bucket, region, key, endpoint)
}

store_gcp_bucket <- function(path) {
  # compatibility with targets <= 0.8.1:
  if (store_gcp_path_0.8.1(path)) {
    return(path[1L])
  }
  # with metadata written by targets > 0.8.1:
  store_gcp_path_field(path = path, pattern = "^bucket=")
}

store_gcp_region <- function(path) {
  # compatibility with targets <= 0.8.1:
  if (store_gcp_path_0.8.1(path)) {
    return(NULL)
  }
  # with metadata written by targets > 0.8.1:
  out <- store_gcp_path_field(path = path, pattern = "^region=")
  out <- if_any(length(out) > 0L && any(nzchar(out)), out, "")
  if_any(identical(out, "NULL"), NULL, out)
}

store_gcp_endpoint <- function(path) {
  # compatibility with targets <= 0.8.1:
  if (store_gcp_path_0.8.1(path)) {
    return(NULL)
  }
  # with metadata written by targets > 0.8.1:
  out <- store_gcp_path_field(path = path, pattern = "^endpoint=")
  out <- if_any(length(out) > 0L && any(nzchar(out)), out, "")
  out <- base64url::base64_urldecode(out)
  if_any(identical(out, "NULL"), NULL, out)
}

store_gcp_key <- function(path) {
  # compatibility with targets <= 0.8.1:
  if (store_gcp_path_0.8.1(path)) {
    return(path[2L])
  }
  store_gcp_path_field(path = path, pattern = "^key=")
}

store_gcp_path_field <- function(path, pattern) {
  path <- store_gcp_split_colon(path)
  keyvalue_field(x = path, pattern = pattern)
}

store_gcp_version <- function(path) {
  out <- store_gcp_path_field(path = path, pattern = "^version=")
  if_any(length(out) && nzchar(out), out, NULL)
}

store_gcp_path_0.8.1 <- function(path) {
  !any(grepl(pattern = "^bucket=", x = path))
}

# Tech debt from a dev version. Need to be compatible.
store_gcp_split_colon <- function(path) {
  index <- grep(pattern = "^bucket=", x = path)
  bucket_pair <- unlist(strsplit(x = path[index], split = ":"))
  c(bucket_pair, path[-index])
}

# Semi-automated tests of Amazon S3 integration live in tests/gcp/. # nolint
# These tests should not be fully automated because they
# automatically create S3 buckets and upload data,
# which could put an unexpected and unfair burden on
# external contributors from the open source community.
# nocov start
#' @export
store_read_object.tar_gcp <- function(store) {
  path <- store$file$path
  tmp <- tempfile()
  on.exit(unlink(tmp))
  gcp_gcs_download(
    key = store_gcp_key(path),
    bucket = store_gcp_bucket(path),
    file = tmp,
    region = store_gcp_region(path),
    endpoint = store_gcp_endpoint(path),
    version = store_gcp_version(path)
  )
  store_convert_object(store, store_read_path(store, tmp))
}

#' @export
store_exist_object.tar_gcp <- function(store, name = NULL) {
  path <- store$file$path
  gcp_gcs_exists(
    key = store_gcp_key(path),
    bucket = store_gcp_bucket(path),
    region = store_gcp_region(path),
    endpoint = store_gcp_endpoint(path),
    version = store_gcp_version(path)
  )
}

#' @export
store_delete_object.tar_gcp <- function(store, name = NULL) {
  path <- store$file$path
  key <- store_gcp_key(path)
  bucket <- store_gcp_bucket(path)
  region <- store_gcp_region(path)
  endpoint <- store_gcp_endpoint(path)
  version <- store_gcp_version(path)
  message <- paste(
    "could not delete target %s from gcp bucket %s key %s.",
    "Either delete the object manually in the gcp web console",
    "or call tar_invalidate(%s) to prevent the targets package",
    "from trying to delete it.\nMessage: "
  )
  message <- sprintf(message, name, bucket, key, name)
  tryCatch(
    gcp_gcs_delete(
      key = key,
      bucket =  bucket,
      region = region,
      endpoint = endpoint,
      version = version
    ),
    error = function(condition) {
      tar_throw_validate(message, conditionMessage(condition))
    }
  )
}

#' @export
store_upload_object.tar_gcp <- function(store) {
  key <- store_gcp_key(store$file$path)
  head <- if_any(
    file_exists_stage(store$file),
    gcp_gcs_upload(
      file = store$file$stage,
      key = key,
      bucket = store_gcp_bucket(store$file$path),
      region = store_gcp_region(store$file$path),
      endpoint = store_gcp_endpoint(store$file$path),
      metadata = list("targets-hash" = store$file$hash),
      part_size = store$resources$gcp$part_size %|||% (5 * (2 ^ 20))
    ),
    tar_throw_file(
      "Cannot upload non-existent gcp staging file ",
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
store_has_correct_hash.tar_gcp <- function(store) {
  path <- store$file$path
  bucket <- store_gcp_bucket(path)
  region <- store_gcp_region(path)
  endpoint <- store_gcp_endpoint(path)
  key <- store_gcp_key(path)
  version <- store_gcp_version(path)
  if_any(
    gcp_gcs_exists(
      key = key,
      bucket = bucket,
      region = region,
      endpoint = endpoint,
      version = version
    ),
    identical(
      store_gcp_hash(
        key = key,
        bucket = bucket,
        region = region,
        endpoint = endpoint,
        version = version
      ),
      store$file$hash
    ),
    FALSE
  )
}

store_gcp_hash <- function(key, bucket, region, endpoint, version) {
  head <- gcp_gcs_head(
    key = key,
    bucket = bucket,
    region = region,
    endpoint = endpoint,
    version = version
  )
  head$Metadata[["targets-hash"]]
}
# nocov end

#' @export
store_get_packages.tar_gcp <- function(store) {
  c("pgcp", NextMethod())
}
