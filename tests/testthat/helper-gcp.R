skip_if_no_gcp <- function() {
  skip_if(Sys.getenv("GCS_AUTH_FILE") == "")
  skip_if_not_installed("googleCloudStorageR")
  skip_if_offline()
  skip_on_cran()
}

gcp_gcs_delete_bucket <- function(bucket) {
  gcp_gcs_auth()
  googleCloudStorageR::gcs_delete_bucket(bucket, force_delete = TRUE)
}

aws_s3_delete_targets_buckets <- function() {
  gcp_gcs_auth()
  project <- Sys.getenv("GCE_DEFAULT_PROJECT_ID")
  buckets <- googleCloudStorageR::gcs_list_buckets(projectId = project)$name
  buckets <- grep(
    pattern = "^targets-test-bucket-",
    x = buckets,
    value = TRUE
  )
  for (bucket in buckets) {
    message(bucket)
    gcp_gcs_delete_bucket(bucket)
  }
}
