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

gcp_gcs_delete_bucket <- function(bucket){
  # have to delete all objects in a bucket first
  os <- googleCloudStorageR::gcs_list_objects(
    bucket, 
    versions = TRUE, 
    detail = "full")
  
  safe_delete <- function(x, bucket, version = NULL){
    tryCatch({
      googleCloudStorageR::gcs_delete_object(x, 
                                             bucket = bucket,
                                             generation = version)
    }, error = function(ex) {
      NULL
    })
  }
  
  lapply(os$name, safe_delete, bucket = bucket)
  mapply(safe_delete, 
         x = os$name, version = os$generation, 
         MoreArgs = list(bucket = bucket))
  
  bb <- googleCloudStorageR::gcs_get_bucket(bucket)
  
  googleCloudStorageR::gcs_delete_bucket(bb$name)
  
}