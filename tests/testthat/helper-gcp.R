skip_if_no_gcp <- function() {
  skip_if(Sys.getenv("GCS_AUTH_FILE") == "")
  skip_if_not_installed("googleCloudStorageR")
  skip_if_offline()
  skip_on_cran()
}

auth_gcp <- function() {
  # the auth service email in this file needs GCP "Storage Admin" IAM role
  googleCloudStorageR::gcs_auth(Sys.getenv("GCS_AUTH_FILE"))
}

gcp_gcs_delete_bucket <- function(bucket) {

  # force_delete in v0.7.0
  googleCloudStorageR::gcs_delete_bucket(bucket, force_delete = TRUE)

}
