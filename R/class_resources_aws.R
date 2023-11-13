resources_aws_init <- function(
  bucket = NULL,
  prefix = path_objects_dir_cloud(),
  region = NULL,
  endpoint = NULL,
  s3_force_path_style = NULL,
  part_size = 5 * (2 ^ 20),
  page_size = 1000L,
  max_tries = NULL,
  seconds_timeout = NULL,
  close_connection = NULL,
  verbose = TRUE,
  args = list()
) {
  resources_aws_new(
    bucket = bucket,
    prefix = prefix,
    region = region,
    endpoint = endpoint,
    s3_force_path_style = s3_force_path_style,
    part_size = part_size,
    page_size = page_size,
    max_tries = max_tries,
    seconds_timeout = seconds_timeout,
    close_connection = close_connection,
    verbose = verbose,
    args = args
  )
}

resources_aws_new <- function(
  bucket = NULL,
  prefix = NULL,
  region = NULL,
  endpoint = NULL,
  s3_force_path_style = NULL,
  part_size = NULL,
  page_size = NULL,
  max_tries = NULL,
  seconds_timeout = NULL,
  close_connection = NULL,
  verbose = TRUE,
  args = NULL
) {
  force(bucket)
  force(prefix)
  force(region)
  force(endpoint)
  force(s3_force_path_style)
  force(part_size)
  force(page_size)
  force(max_tries)
  force(seconds_timeout)
  force(close_connection)
  force(verbose)
  force(args)
  enclass(environment(), c("tar_resources_aws", "tar_resources"))
}

#' @export
resources_validate.tar_resources_aws <- function(resources) {
  for (field in c("bucket", "prefix")) {
    msg <- paste("invalid AWS S3", field)
    tar_assert_scalar(resources[[field]], msg = msg)
    tar_assert_chr(resources[[field]], msg = msg)
    tar_assert_none_na(resources[[field]], msg = msg)
    tar_assert_nzchar(resources[[field]], msg = msg)
  }
  for (field in c("region", "endpoint")) {
    msg <- paste("invalid AWS S3", field)
    tar_assert_scalar(resources[[field]] %|||% "x", msg = msg)
    tar_assert_chr(resources[[field]] %|||% "x", msg = msg)
    tar_assert_none_na(resources[[field]] %|||% "x", msg = msg)
  }
  for (field in c("part_size", "page_size", "max_tries", "seconds_timeout")) {
    msg <- paste("invalid AWS S3", field)
    tar_assert_scalar(resources[[field]] %|||% 1L, msg = msg)
    tar_assert_dbl(resources[[field]] %|||% 1L, msg = msg)
    tar_assert_none_na(resources[[field]] %|||% 1L, msg = msg)
    tar_assert_ge(resources[[field]] %|||% 1L, 0L, msg = msg)
  }
  for (field in c("close_connection", "s3_force_path_style", "verbose")) {
    msg <- paste("invalid AWS S3", field)
    tar_assert_scalar(resources[[field]] %|||% TRUE, msg = msg)
    tar_assert_lgl(resources[[field]] %|||% TRUE, msg = msg)
    tar_assert_none_na(resources[[field]] %|||% TRUE, msg = msg)
  }
  resources_aws_validate_args(resources$args)
}

resources_aws_validate_args <- function(args) {
  tar_assert_list(args)
  tar_assert_named(
    args,
    "informal args of tar_resources_aws() must have nonempty unique names."
  )
  names <- names(args)
  illegal <- c(
    setdiff(names(formals(tar_resources_aws)), "..."),
    "bucket", "Bucket", "key", "Key",
    "prefix", "region", "part_size", "endpoint",
    "VersionId", "body", "Body",
    "metadata", "Metadata", "UploadId", "MultipartUpload",
    "PartNumber"
  )
  illegal <- unique(illegal)
  msg <- paste(
    "arguments to tar_resources_aws() must not include:",
    paste(illegal, collapse = ", ")
  )
  tar_assert_not_in(names, illegal, msg = msg)
  client <- paws.storage::s3()
  legal <- c(
    names(formals(client$head_object)),
    names(formals(client$get_object)),
    names(formals(client$delete_object)),
    names(formals(client$put_object)),
    names(formals(client$create_multipart_upload)),
    names(formals(client$abort_multipart_upload)),
    names(formals(client$complete_multipart_upload)),
    names(formals(client$upload_part))
  )
  legal <- unique(legal)
  msg <- paste(
    "informal arguments (via ...) in tar_resources_aws() must be:",
    "arguments to paws.storage::s3() functions: head_object(), get_object(),",
    "delete_object(), put_object(), create_multipart_upload(),",
    "abort_multipart_upload(), complete_multipart_upload(),",
    "or upload_part(). Documentation of these functions,",
    "including lists and explanations of the arguments,",
    "are linked from https://www.paws-r-sdk.com/docs/s3"
  )
  tar_assert_in(names, legal, msg)
}

#' @export
print.tar_resources_aws <- function(x, ...) {
  cat(
    "<tar_resources_aws>\n ",
    paste0(paste_list(as.list(x)), collapse = "\n  ")
  )
}
