#' @title Target resources: AWS storage formats
#' @export
#' @family resources
#' @description Create the `aws` argument of `tar_resources()`
#'   to specify optional settings to AWS storage formats.
#'   See the `format` argument of [tar_target()] for details.
#' @inheritSection tar_resources Resources
#' @return Object of class `"tar_resources_aws"`, to be supplied
#'   to the `aws` argument of `tar_resources()`.
#' @param bucket Character of length 1, name of an existing
#'   AWS S3 bucket to upload and download the return values
#'   of the affected targets during the pipeline.
#' @param prefix Character of length 1, "directory path"
#'   in the S3 bucket where the target return values are stored.
#' @examples
#' # Somewhere in you target script file (usually _targets.R):
#' tar_target(
#'   name,
#'   command(),
#'   format = "aws_qs",
#'   resources = tar_resources(
#'     aws = tar_resources_aws(bucket = "yourbucketname"),
#'     qs = tar_resources_qs(preset = "fast")
#'   )
#' )
tar_resources_aws <- function(
  bucket,
  prefix = targets::path_objects_dir_cloud()
) {
  out <- resources_aws_init(
    bucket = bucket,
    prefix = prefix
  )
  resources_validate(out)
  out
}
