#' @title Target resources: Google Cloud Platform (GCP)
#'   Google Cloud Storage (GCS)
#' @export
#' @family resources
#' @description Create the `gcp` argument of `tar_resources()`
#'   to specify optional settings for Google Cloud Storage for
#'   targets with `tar_target(..., repository = "gcp")`.
#'   See the `format` argument of [tar_target()] for details.
#' @details See the cloud storage section of
#'   <https://books.ropensci.org/targets/data.html>
#'   for details for instructions.
#' @inheritSection tar_resources Resources
#' @return Object of class `"tar_resources_gcp"`, to be supplied
#'   to the `gcp` argument of `tar_resources()`.
#' @inheritParams tar_resources_aws
#' @param predefined_acl Character of length 1, user access
#'   to the object. See `?googleCloudStorageR::gcs_upload`
#'   for possible values.
#' @param verbose Logical of length 1, whether to print
#'   extra messages like progress bars during uploads
#'   and downloads.
#' @examples
#' # Somewhere in you target script file (usually _targets.R):
#' tar_target(
#'   name,
#'   command(),
#'   format = "qs",
#'   repository = "gcp",
#'   resources = tar_resources(
#'     gcp = tar_resources_gcp(bucket = "yourbucketname"),
#'     qs = tar_resources_qs(preset = "fast")
#'   )
#' )
tar_resources_gcp <- function(
  bucket,
  prefix = targets::path_objects_dir_cloud(),
  predefined_acl = "private",
  verbose = FALSE
) {
  out <- resources_gcp_init(
    bucket = bucket,
    prefix = prefix,
    predefined_acl = predefined_acl,
    verbose = verbose
  )
  resources_validate(out)
  out
}
