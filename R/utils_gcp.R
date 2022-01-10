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
  version = NULL
) {
  tryCatch(
    gcp_gcs_head_true(
      key = key,
      bucket = bucket,
      version = version
    ),
    http_404 = function(condition) {
      FALSE
    }
  )
}

# to keep lines shorter
gcp_gcs_bucket <- function() {
  googleCloudStorageR::gcs_get_global_bucket()
}

gcp_gcs_head <- function(
  key,
  bucket = gcp_gcs_bucket(),
  version = NULL,
  verbose = FALSE
) {
  loud <- if_any(verbose, identity, suppressMessages)
  loud(
    googleCloudStorageR::gcs_get_object(
      key,
      bucket = bucket,
      meta = TRUE,
      generation = version
    )
  )
}

gcp_gcs_head_true <- function(
  key,
  bucket = gcp_gcs_bucket(),
  version = NULL
) {
  gcp_gcs_head(
    key = key,
    bucket = bucket,
    version = version
  )
  TRUE
}

gcp_gcs_download <- function(
  file,
  key,
  bucket = gcp_gcs_bucket(),
  version = NULL
) {
  googleCloudStorageR::gcs_get_object(
    key,
    bucket = bucket,
    saveToDisk = file,
    overwrite = TRUE,
    generation = version
  )
}

gcp_gcs_upload <- function(
  file,
  key,
  bucket = gcp_gcs_bucket(),
  metadata = list(),
  predefined_acl = c(
    "private", "bucketLevel", "authenticatedRead",
    "bucketOwnerFullControl", "bucketOwnerRead",
    "projectPrivate", "publicRead", "default"
  )
) {
  predefined_acl <- match.arg(predefined_acl)
  meta <- NULL
  if (length(metadata) > 0) {
    meta <- googleCloudStorageR::gcs_metadata_object(
      object_name = key,
      metadata = metadata
    )
  }
  googleCloudStorageR::gcs_upload(
    file,
    bucket = bucket,
    name = key,
    object_metadata = meta,
    predefinedAcl = predefined_acl
  )
}
# nocov end
