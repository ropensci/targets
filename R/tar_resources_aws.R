#' @title Target resources: Amazon Web Services (AWS) S3 storage
#' @export
#' @family resources
#' @description Create the `aws` argument of `tar_resources()`
#'   to specify optional settings to AWS for
#'   `tar_target(..., repository = "aws")`.
#'   See the `format` argument of [tar_target()] for details.
#' @details See the cloud storage section of
#'   <https://books.ropensci.org/targets/data.html>
#'   for details for instructions.
#' @inheritSection tar_resources Resources
#' @return Object of class `"tar_resources_aws"`, to be supplied
#'   to the `aws` argument of `tar_resources()`.
#' @param bucket Character of length 1, name of an existing
#'   bucket to upload and download the return values
#'   of the affected targets during the pipeline.
#' @param prefix Character of length 1, "directory path"
#'   in the bucket where the target return values are stored.
#'   Defaults to `targets::tar_path_objects_dir_cloud()`.
#' @param region Character of length 1, AWS region containing the S3 bucket.
#'   Set to `NULL` to use the default region.
#' @param part_size Positive numeric of length 1, number of bytes
#'   for each part of a multipart upload. (Except the last part,
#'   which is the remainder.) In a multipart upload, each part
#'   must be at least 5 MB. The default value of the `part_size`
#'   argument is `5 * (2 ^ 20)`.
#' @param endpoint Character of length 1, URL endpoint for S3 storage.
#'   Defaults to the Amazon AWS endpoint if `NULL`. Example:
#'   To use the S3 protocol with Google Cloud Storage,
#'   set `endpoint = "https://storage.googleapis.com"`
#'   and `region = "auto"`. Also make sure to create
#'   HMAC access keys in the Google Cloud Storage console
#'   (under Settings => Interoperability) and set the
#'   `AWS_ACCESS_KEY_ID` and `AWS_SECRET_ACCESS_KEY` environment
#'   variables accordingly. After that, you should be able to use
#'   S3 storage formats with Google Cloud storage buckets.
#'   There is one limitation, however: even if your bucket has
#'   object versioning turned on, `targets` may fail to record object
#'   versions. Google Cloud Storage in particular has this
#'   incompatibility.
#' @param ... Named arguments to functions in `paws.storage::s3()` to manage
#'   S3 storage. The documentation of these specific functions
#'   is linked from `https://www.paws-r-sdk.com/docs/s3/`.
#'   The configurable functions themselves are:
#'   * `paws.storage::s3()$head_object()`
#'   * `paws.storage::s3()$get_object()`
#'   * `paws.storage::s3()$delete_object()`
#'   * `paws.storage::s3()$put_object()`
#'   * `paws.storage::s3()$create_multipart_upload()`
#'   * `paws.storage::s3()$abort_multipart_upload()`
#'   * `paws.storage::s3()$complete_multipart_upload()`
#'   * `paws.storage::s3()$upload_part()`
#'   The named arguments in `...` must not be any of
#'   `"bucket"`, `"Bucket"`, `"key"`, `"Key"`,
#'   `"prefix"`, `"region"`, `"part_size"`, `"endpoint"`,
#'   `"version"`, `"VersionId"`, `"body"`, `"Body"`,
#'   `"metadata"`, `"Metadata"`, `"UploadId"`, `"MultipartUpload"`,
#'   or `"PartNumber"`.
#' @examples
#' # Somewhere in you target script file (usually _targets.R):
#' if (identical(Sys.getenv("TAR_EXAMPLES"), "true")) { # for CRAN
#' tar_target(
#'   name,
#'   command(),
#'   format = "qs",
#'   repository = "aws",
#'   resources = tar_resources(
#'     aws = tar_resources_aws(bucket = "yourbucketname"),
#'     qs = tar_resources_qs(preset = "fast")
#'   )
#' )
#' }
tar_resources_aws <- function(
  bucket = targets::tar_option_get("resources")$aws$bucket,
  prefix = targets::tar_option_get("resources")$aws$prefix,
  region = targets::tar_option_get("resources")$aws$region,
  part_size = targets::tar_option_get("resources")$aws$part_size,
  endpoint = targets::tar_option_get("resources")$aws$endpoint,
  ...
) {
  prefix <- prefix %|||% targets::tar_path_objects_dir_cloud()
  part_size <- part_size %|||% (5 * (2 ^ 20))
  args <- list(...)
  default_args <- targets::tar_option_get("resources")$aws$args
  for (name in names(default_args)) {
    args[[name]] <- args[[name]] %|||% default_args[[name]]
  }
  out <- resources_aws_init(
    bucket = bucket,
    prefix = prefix,
    region = region,
    part_size = part_size,
    endpoint = endpoint,
    args = args
  )
  resources_validate(out)
  out
}
