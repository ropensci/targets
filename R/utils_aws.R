# Semi-automated tests of Amazon S3 integration live in tests/aws/. # nolint
# These tests should not be fully automated because they
# automatically create S3 buckets and upload data,
# which could put an unexpected and unfair burden on
# external contributors from the open source community.
# nocov start
aws_exists <- function(key, bucket, region = NULL, version = NULL) {
  tryCatch(
    aws_head_true(
      key = key,
      bucket = bucket,
      region = region,
      version = version
    ),
    http_404 = function(condition) {
      FALSE
    }
  )
}

aws_head <- function(key, bucket, region = NULL, version = NULL) {
  if (!is.null(region)) {
    withr::local_envvar(.new = list(AWS_REGION = region))
  }
  args <- list(
    Key = key,
    Bucket = bucket
  )
  if (!is.null(version)) {
    args$VersionId <- version
  }
  do.call(what = paws::s3()$head_object, args = args)
}

aws_head_true <- function(key, bucket, region = NULL, version = NULL) {
  aws_head(
    key = key,
    bucket = bucket,
    region = region,
    version = version
  )
  TRUE
}

aws_download <- function(
  file,
  key,
  bucket,
  region = NULL,
  version = NULL
) {
  if (!is.null(region)) {
    withr::local_envvar(.new = list(AWS_REGION = region))
  }
  args <- list(
    Key = key,
    Bucket = bucket
  )
  if (!is.null(version)) {
    args$VersionId <- version
  }
  out <- do.call(what = paws::s3()$get_object, args = args)$Body
  writeBin(out, con = file)
}

aws_upload <- function(
  file,
  key,
  bucket,
  region = NULL,
  metadata = list(),
  multipart = file.size(file) > part_size,
  part_size = 5 * (2 ^ 20)
) {
  if (!is.null(region)) {
    withr::local_envvar(.new = list(AWS_REGION = region))
  }
  client <- paws::s3()
  if (!multipart) {
    out <- client$put_object(
      Body = readBin(file, what = "raw", n = file.size(file)),
      Key = key,
      Bucket = bucket,
      Metadata = metadata
    )
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
    parts <- aws_upload_parts(
      file = file,
      key = key,
      bucket = bucket,
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
  return(response)
}

aws_upload_parts <- function(
  file,
  key,
  bucket,
  part_size,
  upload_id
) {
  client <- paws::s3()
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
  return(parts)
}

aws_version <- function(key, bucket, region = NULL) {
  head <- aws_head(
    key = key,
    bucket = bucket,
    region = region
  )
  attr(head, "x-amz-version-id")
}
# nocov end
