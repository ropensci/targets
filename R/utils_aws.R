# Semi-automated tests of Amazon S3 integration live in tests/aws/. # nolint
# These tests should not be fully automated because they
# automatically create S3 buckets and upload data,
# which could put an unexpected and unfair burden on
# external contributors from the open source community.
# nocov start
aws_s3_exists <- function(
  key,
  bucket,
  region = NULL,
  endpoint = NULL,
  version = NULL
) {
  tryCatch(
    aws_s3_head_true(
      key = key,
      bucket = bucket,
      region = region,
      endpoint = endpoint,
      version = version
    ),
    http_400 = function(condition) {
      FALSE
    }
  )
}

aws_s3_head <- function(
  key,
  bucket,
  region = NULL,
  endpoint = NULL,
  version = NULL
) {
  client <- aws_s3_client(endpoint = endpoint, region = region)
  args <- list(
    Key = key,
    Bucket = bucket
  )
  if (!is.null(version)) {
    args$VersionId <- version
  }
  do.call(what = client$head_object, args = args)
}

aws_s3_head_true <- function(
  key,
  bucket,
  region = NULL,
  endpoint = NULL,
  version = NULL
) {
  aws_s3_head(
    key = key,
    bucket = bucket,
    region = region,
    endpoint = endpoint,
    version = version
  )
  TRUE
}

aws_s3_download <- function(
  file,
  key,
  bucket,
  region = NULL,
  endpoint = NULL,
  version = NULL
) {
  client <- aws_s3_client(endpoint = endpoint, region = region)
  args <- list(
    Key = key,
    Bucket = bucket
  )
  if (!is.null(version)) {
    args$VersionId <- version
  }
  out <- do.call(what = client$get_object, args = args)$Body
  writeBin(out, con = file)
  invisible()
}

aws_s3_delete <- function(
  key,
  bucket,
  region = NULL,
  endpoint = NULL,
  version = NULL
) {
  client <- aws_s3_client(endpoint = endpoint, region = region)
  args <- list(
    Key = key,
    Bucket = bucket
  )
  if (!is.null(version)) {
    args$VersionId <- version
  }
  do.call(what = client$delete_object, args = args)
  invisible()
}

# Copied from https://github.com/paws-r/paws/blob/main/examples/s3_multipart_upload.R # nolint
# and modified under Apache 2.0.
# See the NOTICE file at the top of this package for attribution.
aws_s3_upload <- function(
  file,
  key,
  bucket,
  region = NULL,
  endpoint = NULL,
  metadata = list(),
  multipart = file.size(file) > part_size,
  part_size = 5 * (2 ^ 20)
) {
  client <- aws_s3_client(endpoint = endpoint, region = region)
  if (!multipart) {
    out <- client$put_object(
      Body = readBin(file, what = "raw", n = file.size(file)),
      Key = key,
      Bucket = bucket,
      Metadata = metadata
    )
    return(out)
  }
  multipart <- client$create_multipart_upload(
    Bucket = bucket,
    Key = key,
    Metadata = metadata
  )
  response <- NULL
  on.exit({
    if (is.null(response) || inherits(response, "try-error")) {
      client$abort_multipart_upload(
        Bucket = bucket,
        Key = key,
        UploadId = multipart$UploadId
      )
      tar_throw_file(response)
    }
  })
  response <- try({
    parts <- aws_s3_upload_parts(
      file = file,
      key = key,
      bucket = bucket,
      client = client,
      part_size = part_size,
      upload_id = multipart$UploadId
    )
    client$complete_multipart_upload(
      Bucket = bucket,
      Key = key,
      MultipartUpload = list(Parts = parts),
      UploadId = multipart$UploadId
    )
  }, silent = TRUE)
  response
}

# Copied from https://github.com/paws-r/paws/blob/main/examples/s3_multipart_upload.R # nolint
# and modified under Apache 2.0.
# See the NOTICE file at the top of this package for attribution.
aws_s3_upload_parts <- function(
  file,
  key,
  bucket,
  client,
  part_size,
  upload_id
) {
  file_size <- file.size(file)
  num_parts <- ceiling(file_size / part_size)
  con <- base::file(file, open = "rb")
  on.exit(close(con))
  parts <- list()
  for (i in seq_len(num_parts)) {
    cli_blue_bullet(sprintf("upload %s part %s of %s", file, i, num_parts))
    part <- readBin(con, what = "raw", n = part_size)
    part_response <- client$upload_part(
      Body = part,
      Bucket = bucket,
      Key = key,
      PartNumber = i,
      UploadId = upload_id
    )
    parts <- c(parts, list(list(ETag = part_response$ETag, PartNumber = i)))
  }
  parts
}

aws_s3_client <- function(endpoint, region) {
  config <- list()
  if (!is.null(endpoint)) {
    config$endpoint <- endpoint
  }
  if (!is.null(region)) {
    config$region <- region
  }
  paws::s3(config = config)
}
# nocov end
