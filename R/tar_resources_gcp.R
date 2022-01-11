#' @title Target resources: GCP storage formats
#' @export
#' @family resources
#' @description Create the `gcp` argument of `tar_resources()`
#'   to specify optional settings to gcp storage formats.
#'   See the `format` argument of [tar_target()] for details.
#' @inheritSection tar_resources Resources
#' @return Object of class `"tar_resources_gcp"`, to be supplied
#'   to the `gcp` argument of `tar_resources()`.
#' @param bucket Character of length 1, name of an existing
#'   gcp bucket to upload and download the return values
#'   of the affected targets during the pipeline.
#' @param prefix Character of length 1, "directory path"
#'   in the S3 bucket where the target return values are stored.
#' @param verbose Whether to have feedback on the GCS upload process
#' @examples
#' # Somewhere in you target script file (usually _targets.R):
#' tar_target(
#'   name,
#'   command(),
#'   format = "gcp_qs",
#'   resources = tar_resources(
#'     gcp = tar_resources_gcp(bucket = "yourbucketname"),
#'     qs = tar_resources_qs(preset = "fast")
#'   )
#' )
tar_resources_gcp <- function(
  bucket,
  prefix = targets::path_objects_dir_cloud(),
  verbose = FALSE
) {
  out <- resources_gcp_init(
    bucket = bucket,
    prefix = prefix,
    verbose = verbose
  )
  resources_validate(out)
  out
}
