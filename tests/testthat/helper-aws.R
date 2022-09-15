skip_if_no_aws <- function() {
  skip_if(Sys.getenv("AWS_ACCESS_KEY_ID") == "")
  skip_if(Sys.getenv("AWS_SECRET_ACCESS_KEY") == "")
  skip_if(Sys.getenv("AWS_REGION") == "")
  skip_if_not_installed("paws")
  skip_if_offline()
  skip_cran()
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

aws_s3_delete_bucket <- function(bucket, client = paws::s3()) {
  region <- client$get_bucket_location(Bucket = bucket)
  withr::local_envvar(.new = list(AWS_REGION = region))
  out <- client$list_object_versions(Bucket = bucket)
  for (x in out$Versions) {
    args <- list(
      Bucket = bucket,
      Key = x$Key
    )
    has_version_id <- !identical(x$VersionId, "null") &&
      !identical(x$VersionId, character(0))
    if (has_version_id) {
      args$VersionId <- x$VersionId
    }
    do.call(client$delete_object, args)
  }
  client$delete_bucket(Bucket = bucket)
}

aws_s3_delete_targets_buckets <- function(client = paws::s3()) {
  out <- client$list_buckets()
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
    aws_s3_delete_bucket(bucket, client = client)
  }
}
