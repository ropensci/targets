resources_aws_init <- function(
  bucket = NULL,
  prefix = tar_path_objects_dir_cloud(),
  region = NULL,
  part_size = 5 * (2 ^ 20),
  endpoint = NULL,
  args = list()
) {
  resources_aws_new(
    bucket = bucket,
    prefix = prefix,
    region = region,
    part_size = part_size,
    endpoint = endpoint,
    args = args
  )
}

resources_aws_new <- function(
  bucket = NULL,
  prefix = NULL,
  region = NULL,
  part_size = NULL,
  endpoint = NULL,
  args = NULL
) {
  force(bucket)
  force(prefix)
  force(region)
  force(part_size)
  force(endpoint)
  force(args)
  enclass(environment(), c("tar_resources_aws", "tar_resources"))
}

#' @export
resources_validate.tar_resources_aws <- function(resources) {
  tar_assert_scalar(resources$bucket %|||% "bucket")
  tar_assert_chr(resources$bucket %|||% "bucket")
  tar_assert_nzchar(resources$bucket %|||% "bucket")
  tar_assert_scalar(resources$prefix)
  tar_assert_chr(resources$prefix)
  tar_assert_nzchar(resources$prefix)
  tar_assert_scalar(resources$region %|||% "region")
  tar_assert_chr(resources$region %|||% "region")
  tar_assert_scalar(resources$part_size %|||% 1e8)
  tar_assert_dbl(resources$part_size %|||% 1e8)
  tar_assert_positive(resources$part_size %|||% 1e8)
  tar_assert_scalar(resources$endpoint %|||% "endpoint")
  tar_assert_chr(resources$endpoint %|||% "endpoint")
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
    "version", "VersionId", "body", "Body",
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
