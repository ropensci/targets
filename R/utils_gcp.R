# Semi-automated tests of Google Cloud Storage
# integration live in tests/gcp/.
# These tests should not be fully automated because they
# automatically create S3 buckets and upload data,
# which could put an unexpected and unfair burden on
# external contributors from the open source community.
# nocov start
gcp_gcs_exists <- function(
  key,
  bucket = gcp_gcs_bucket(),
  version = NULL,
  verbose = FALSE
) {
  tryCatch(
    gcp_gcs_head_true(
      key = key,
      bucket = bucket,
      version = version,
      verbose = verbose
    ),
    http_404 = function(condition) {
      FALSE
    }
  )
}

gcp_gcs_head_true <- function(
  key,
  bucket = gcp_gcs_bucket(),
  version = NULL,
  verbose = FALSE
) {
  gcp_gcs_head(
    key = key,
    bucket = bucket,
    version = version,
    verbose = verbose
  )
  TRUE
}

gcp_gcs_head <- function(
  key,
  bucket = gcp_gcs_bucket(),
  version = NULL,
  verbose = FALSE
) {
  gcp_gcs_auth(verbose = verbose)
  if_any(verbose, identity, suppressMessages) (
    googleCloudStorageR::gcs_get_object(
      key,
      bucket = bucket,
      meta = TRUE,
      generation = version
    )
  )
}

gcp_gcs_bucket <- function(verbose = FALSE) {
  gcp_gcs_auth(verbose = verbose)
  if_any(verbose, identity, suppressMessages) (
    googleCloudStorageR::gcs_get_global_bucket()
  )
}

gcp_gcs_download <- function(
  file,
  key,
  bucket = gcp_gcs_bucket(),
  version = NULL,
  verbose = FALSE
) {
  gcp_gcs_auth(verbose = verbose)
  if_any(verbose, identity, suppressMessages) (
    googleCloudStorageR::gcs_get_object(
      key,
      bucket = bucket,
      saveToDisk = file,
      overwrite = TRUE,
      generation = version
    )
  )
}

gcp_gcs_delete <- function(
  key,
  bucket = gcp_gcs_bucket(),
  version = NULL,
  verbose = FALSE
) {
  gcp_gcs_auth(verbose = verbose)
  exists <- gcp_gcs_exists(
    key = key,
    bucket = bucket,
    version = version,
    verbose = verbose
  )
  if (!exists) {
    return()
  }
  if_any(verbose, identity, suppressMessages) (
    googleCloudStorageR::gcs_delete_object(
      object_name = key,
      bucket = bucket,
      generation = version
    )
  )
}

gcp_gcs_upload <- function(
  file,
  key,
  bucket = gcp_gcs_bucket(),
  metadata = list(),
  predefined_acl = "private",
  verbose = FALSE
) {
  gcp_gcs_auth(verbose = verbose)
  meta <- NULL
  if (length(metadata) > 0) {
    meta <- googleCloudStorageR::gcs_metadata_object(
      object_name = key,
      metadata = metadata
    )
  }
  if_any(verbose, identity, suppressMessages) (
    googleCloudStorageR::gcs_upload(
      file,
      bucket = bucket,
      name = key,
      object_metadata = meta,
      predefinedAcl = predefined_acl
    )
  )
}

gcp_gcs_auth <- function(verbose = FALSE) {
  if_any(verbose, identity, suppressMessages) (
    googleCloudStorageR::gcs_auth(Sys.getenv("GCS_AUTH_FILE"))
  )
}
# nocov end
