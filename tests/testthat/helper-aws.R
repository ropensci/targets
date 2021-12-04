skip_if_no_aws <- function() {
  skip_if(Sys.getenv("AWS_ACCESS_KEY_ID") == "")
  skip_if(Sys.getenv("AWS_SECRET_ACCESS_KEY") == "")
  skip_if(Sys.getenv("AWS_REGION") == "")
  skip_if_not_installed("paws")
  skip_if_offline()
  skip_on_cran()
}

random_bucket_name <- function() {
  paste0(
    "targets-test-bucket-",
    substr(
      digest::digest(tempfile(), algo = "sha256"),
      start = 0,
      stop = 43
    )
  )
}

destroy_bucket <- function(bucket) {
  s3 <- paws::s3()
  region <- s3$get_bucket_location(Bucket = bucket)
  withr::local_envvar(.new = list(AWS_REGION = region))
  out <- s3$list_object_versions(Bucket = bucket)
  for (x in out$Versions) {
    args <- list(
      Bucket = bucket,
      Key = x$Key
    )
    if (x$VersionId != "null") {
      args$VersionId <- x$VersionId
    }
    do.call(s3$delete_object, args)
  }
  s3$delete_bucket(Bucket = bucket)
}

destroy_targets_buckets <- function() {
  s3 <- paws::s3()
  out <- s3$list_buckets()
  buckets <- unlist(
    lapply(
      out$Buckets,
      function(x) {
        x$Name
      }
    )
  )
  buckets <- grep(
    pattern = "^targets-test-bucket-",
    x = buckets,
    value = TRUE
  )
  for (bucket in buckets) {
    message(bucket)
    destroy_bucket(bucket)
  }
}
