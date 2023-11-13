# Semi-automated tests of Amazon S3 integration live in tests/aws/. # nolint
# These tests should not be fully automated because they
# automatically create S3 buckets and upload data,
# which could put an unexpected and unfair burden on
# external contributors from the open source community.
# nocov start
aws_s3_head <- function(
  key,
  bucket,
  region = NULL,
  endpoint = NULL,
  version = NULL,
  args = list(),
  max_tries = NULL,
  seconds_timeout = NULL,
  close_connection = NULL,
  s3_force_path_style = NULL
) {
  client <- aws_s3_client(
    endpoint = endpoint,
    region = region,
    seconds_timeout = seconds_timeout,
    close_connection = close_connection,
    s3_force_path_style = s3_force_path_style,
    max_tries = max_tries
  )
  args$Key <- key
  args$Bucket <- bucket
  if (!is.null(version)) {
    args$VersionId <- version
  }
  args <- supported_args(fun = client$head_object, args = args)
  tryCatch(
    do.call(what = client$head_object, args = args),
    http_400 = function(condition) NULL
  )
}

aws_s3_exists <- function(
  key,
  bucket,
  region = NULL,
  endpoint = NULL,
  version = NULL,
  args = list(),
  max_tries = NULL,
  seconds_timeout = NULL,
  close_connection = NULL,
  s3_force_path_style = NULL
) {
  !is.null(
    aws_s3_head(
      key = key,
      bucket = bucket,
      region = region,
      endpoint = endpoint,
      version = version,
      args = args,
      max_tries = max_tries
    )
  )
}

aws_s3_list_etags <- function(
  prefix,
  bucket,
  page_size = 1000L,
  verbose = TRUE,
  region = NULL,
  endpoint = NULL,
  args = list(),
  max_tries = NULL,
  seconds_timeout = NULL,
  close_connection = NULL,
  s3_force_path_style = NULL
) {
  client <- aws_s3_client(
    endpoint = endpoint,
    region = region,
    seconds_timeout = seconds_timeout,
    close_connection = close_connection,
    s3_force_path_style = s3_force_path_style,
    max_tries = max_tries
  )
  args$Bucket <- bucket
  args$Prefix <- prefix
  page_size <- page_size %|||% 1000L
  verbose <- verbose %|||% TRUE
  args <- supported_args(fun = client$list_objects_v2, args = args)
  if (verbose) {
    tar_message_run(
      "Listing objects in AWS S3 bucket ",
      bucket,
      " prefix ",
      prefix
    )
  }
  pages <- paws.common::paginate(
    Operation = do.call(what = client$list_objects_v2, args = args),
    PageSize = page_size
  )
  out <- list()
  for (page in pages) {
    for (object in page$Contents) {
      out[[object$Key]] <- object$ETag
    }
  }
  out
}

aws_s3_download <- function(
  file,
  key,
  bucket,
  region = NULL,
  endpoint = NULL,
  version = NULL,
  args = list(),
  max_tries = NULL,
  seconds_timeout = NULL,
  close_connection = NULL,
  s3_force_path_style = NULL
) {
  client <- aws_s3_client(
    endpoint = endpoint,
    region = region,
    seconds_timeout = seconds_timeout,
    close_connection = close_connection,
    s3_force_path_style = s3_force_path_style,
    max_tries = max_tries
  )
  args$Key <- key
  args$Bucket <- bucket
  if (!is.null(version)) {
    args$VersionId <- version
  }
  dir_create(dirname(file))
  args <- supported_args(fun = client$get_object, args = args)
  out <- do.call(what = client$get_object, args = args)$Body
  writeBin(out, con = file)
  invisible()
}

aws_s3_delete <- function(
  key,
  bucket,
  region = NULL,
  endpoint = NULL,
  version = NULL,
  args = list(),
  max_tries = NULL,
  seconds_timeout = NULL,
  close_connection = NULL,
  s3_force_path_style = NULL
) {
  client <- aws_s3_client(
    endpoint = endpoint,
    region = region,
    seconds_timeout = seconds_timeout,
    close_connection = close_connection,
    s3_force_path_style = s3_force_path_style,
    max_tries = max_tries
  )
  args$Key <- key
  args$Bucket <- bucket
  if (!is.null(version)) {
    args$VersionId <- version
  }
  args <- supported_args(fun = client$delete_object, args = args)
  do.call(what = client$delete_object, args = args)
  invisible()
}

# See https://www.paws-r-sdk.com/docs/s3_delete_objects/
# to format the "objects" argument.
aws_s3_delete_objects <- function(
  objects,
  bucket,
  batch_size = 1000L,
  region = NULL,
  endpoint = NULL,
  args = list(),
  max_tries = NULL,
  seconds_timeout = NULL,
  close_connection = NULL,
  s3_force_path_style = NULL,
  verbose = TRUE
) {
  client <- aws_s3_client(
    endpoint = endpoint,
    region = region,
    seconds_timeout = seconds_timeout,
    close_connection = close_connection,
    s3_force_path_style = s3_force_path_style,
    max_tries = max_tries
  )
  args$Bucket <- bucket
  args <- supported_args(fun = client$delete_objects, args = args)
  while (length(objects)) {
    index <- seq_len(min(length(objects), batch_size))
    args$Delete$Objects <- objects[index]
    args$Delete$Quiet <- TRUE
    if (verbose) {
      tar_message_run(
        "Deleting ",
        length(index),
        " objects from AWS S3 bucket ",
        bucket,
        " ",
        sample(c("-", "\\", "|", "/"), size = 1L)
      )
    }
    do.call(what = client$delete_objects, args = args)
    objects <- objects[-index]
  }
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
  part_size = 5 * (2 ^ 20),
  args = list(),
  max_tries = NULL,
  seconds_timeout = NULL,
  close_connection = NULL,
  s3_force_path_style = NULL
) {
  client <- aws_s3_client(
    endpoint = endpoint,
    region = region,
    seconds_timeout = seconds_timeout,
    close_connection = close_connection,
    s3_force_path_style = s3_force_path_style,
    max_tries = max_tries
  )
  part_size <- part_size %|||% (5 * (2 ^ 20))
  if (!multipart) {
    args_put_object <- args
    args_put_object$Body <- readBin(file, what = "raw", n = file.size(file))
    args_put_object$Key <- key
    args_put_object$Bucket <- bucket
    args_put_object$Metadata <- metadata
    args_put_object <- supported_args(
      fun = client$put_object,
      args = args_put_object
    )
    out <- do.call(what = client$put_object, args = args_put_object)
    return(out)
  }
  args_create_multipart_upload <- args
  args_create_multipart_upload$Bucket <- bucket
  args_create_multipart_upload$Key <- key
  args_create_multipart_upload$Metadata <- metadata
  args_create_multipart_upload <- supported_args(
    fun = client$create_multipart_upload,
    args = args_create_multipart_upload
  )
  multipart <- do.call(
    what = client$create_multipart_upload,
    args = args_create_multipart_upload
  )
  response <- NULL
  on.exit({
    if (is.null(response) || inherits(response, "try-error")) {
      args_abort_multipart_upload <- args
      args_abort_multipart_upload$Bucket <- bucket
      args_abort_multipart_upload$Key <- key
      args_abort_multipart_upload$UploadId <- multipart$UploadId
      args_abort_multipart_upload <- supported_args(
        fun = client$abort_multipart_upload,
        args = args_abort_multipart_upload
      )
      do.call(
        what = client$abort_multipart_upload,
        args = args_abort_multipart_upload
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
      upload_id = multipart$UploadId,
      max_tries = max_tries,
      args = args
    )
    args_complete_multipart_upload <- args
    args_complete_multipart_upload$Bucket <- bucket
    args_complete_multipart_upload$Key <- key
    args_complete_multipart_upload$MultipartUpload <- list(Parts = parts)
    args_complete_multipart_upload$UploadId <- multipart$UploadId
    args_complete_multipart_upload <- supported_args(
      fun = client$complete_multipart_upload,
      args = args_complete_multipart_upload
    )
    do.call(
      what = client$complete_multipart_upload,
      args = args_complete_multipart_upload
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
  upload_id,
  args = list(),
  max_tries
) {
  file_size <- file.size(file)
  num_parts <- ceiling(file_size / part_size)
  con <- base::file(file, open = "rb")
  on.exit(close(con))
  parts <- list()
  for (i in seq_len(num_parts)) {
    cli_blue_bullet(sprintf("upload %s part %s of %s", file, i, num_parts))
    part <- readBin(con, what = "raw", n = part_size)
    args$Body <- part
    args$Bucket <- bucket
    args$Key <- key
    args$PartNumber <- i
    args$UploadId <- upload_id
    args <- supported_args(fun = client$upload_part, args = args)
    part_response <- do.call(what = client$upload_part, args = args)
    parts <- c(parts, list(list(ETag = part_response$ETag, PartNumber = i)))
  }
  parts
}

aws_s3_client <- function(
  endpoint,
  region,
  seconds_timeout,
  close_connection,
  s3_force_path_style,
  max_tries
) {
  config <- list()
  if (!is.null(endpoint)) {
    config$endpoint <- endpoint
  }
  if (!is.null(region)) {
    config$region <- region
  }
  if (!is.null(seconds_timeout)) {
    config$seconds_timeout <- seconds_timeout
  }
  if (!is.null(close_connection)) {
    config$close_connection <- close_connection
  }
  if (!is.null(s3_force_path_style)) {
    config$s3_force_path_style <- s3_force_path_style
  }
  if (!is.null(max_tries)) {
    config$max_retries <- max_tries + 1L
  }
  paws.storage::s3(config = config)
}
# nocov end
