skip_if_no_aws <- function() {
  skip_if(Sys.getenv("AWS_ACCESS_KEY_ID") == "")
  skip_if(Sys.getenv("AWS_SECRET_ACCESS_KEY") == "")
  skip_if(Sys.getenv("AWS_DEFAULT_REGION") == "")
  skip_if_not_installed("aws.s3")
  skip_if_offline()
  skip_on_cran()
}

random_bucket_name <- function() {
  substr(
    digest::digest(tempfile(), algo = "sha256"),
    start = 0,
    stop = 63
  )
}
